import scala.collection.JavaConversions._
import scala.xml.XML
import net.htmlparser.jericho._
// import org.jsoup._
// import org.jsoup.nodes.Element
import org.lorecraft.phparser.SerializedPhpParser
import java.util.{ Date, TimeZone }
import java.text.SimpleDateFormat
import scala.collection.mutable.{ Map => MMap }
import play.api.libs.json.{ JsValue, Json, JsArray, JsNull }
import java.net.URL

object Wp2Prismic {

  // WORDPRESS

  case class WPost(id: String, slug: String, title: String, description: String, content: String, categories: List[WPCategory], tags: List[String], author: WPAuthor, at: Date, comment: Boolean, image: Option[WPImage])

  type WParagraph = String

  case class WPImage(id: String, url: String, width: Option[Int], height: Option[Int], credit: Option[String], copyright: Option[String], alt: Option[String], thumbnails: Seq[WPImage])

  case class WPAuthor(login: String, firstName: String, lastName: String) {
    lazy val fullName = firstName + " " + lastName
  }

  type WPCategory = String

  object WPImage {
    def apply(id: String, url: String, alt: Option[String], description: Option[String], data: String): WPImage = {
      val parser = new SerializedPhpParser(data)
      val parsed = parser.parse()
      val attributes: MMap[String, Any] = parsed.asInstanceOf[java.util.LinkedHashMap[String, Any]]
      val width = attributes.get("width").map(_.asInstanceOf[Int])
      val height = attributes.get("height").map(_.asInstanceOf[Int])
      val meta = attributes.get("image_meta").map { m =>
        val x: MMap[String, Any] = m.asInstanceOf[java.util.LinkedHashMap[String, Any]]
        x
      }
      val credit = meta.flatMap(m => m.get("credit").map(_.asInstanceOf[String])).filter(_ != "")
      val copyright = meta.flatMap(m => m.get("copyright").map(_.asInstanceOf[String])).filter(_ != "")
      val sizes = attributes.get("sizes").map { m =>
        val x: MMap[String, Any] = m.asInstanceOf[java.util.LinkedHashMap[String, Any]]
        x
      } getOrElse MMap.empty
      val thumbnails = Seq("thumbnail", "medium", "post-thumbnail").flatMap { size =>
        sizes.get(size).flatMap { t =>
          val x: MMap[String, Any] = t.asInstanceOf[java.util.LinkedHashMap[String, Any]]
          x.get("file").map(_.asInstanceOf[String]).map { file =>
            val width = x.get("width").map(_.asInstanceOf[Int])
            val height = x.get("height").map(_.asInstanceOf[Int])
            val thumbnailUrl = {
              url.split('/').init.mkString("/") + "/" + file
            }
            WPImage(size, thumbnailUrl, width, height, credit, copyright, alt, Seq.empty)
          }
        }
      }
      WPImage(id, url, width, height, credit, copyright, alt, thumbnails)
    }
  }

  // PRISMIC
  case class Block(text: String, spans: List[Span], openTags: List[(Tag, Int)]) {
    def append(t: String): Block = Block(text + t, spans, openTags)
    def append(c: Char): Block = Block(text + c, spans, openTags)
  }

  object Block {
    def apply(): Block = Block("", Nil, Nil)
    def toJson(block: Block): JsValue =
      Json.obj(
        "type" -> "paragraph",
        "content" -> Json.obj(
          "spans" -> block.spans.map(Span.toJson),
          "text" -> block.text
        )
      )
  }

  case class Span(start: Int, end: Int, typ: String, data: JsValue)

  object Span {

    def toJson(span: Span): JsValue =
      Json.obj(
        "start" -> span.start,
        "end" -> span.end,
        "type" -> span.typ,
        "data" -> span.data
      )

    def strong(start: Int, end: Int): Span =
      Span(start, end, "strong", JsNull)

    def em(start: Int, end: Int): Span =
      Span(start, end, "em", JsNull)

    def hyperlink(start: Int, end: Int, url: String): Span =
      Span(start, end, "hyperlink", Json.obj("url" -> url))

    def label(start: Int, end: Int, data: String): Span =
      Span(start, end, "label", Json.obj("data" -> data))

    def quote(start: Int, end: Int): Span =
      Span(start, end, "quote", JsNull)

    def del(start: Int, end: Int): Span =
      Span(start, end, "del", JsNull)

    def ins(start: Int, end: Int): Span =
      Span(start, end, "ins", JsNull)

    def img(start: Int, end: Int, width: Int, height: Int, url: String): Span =
      Span(start, end, "image", Json.obj(
        "origin" -> Json.obj(
          "height" -> height,
          "width" -> width,
          "url" -> url
        )
      ))
  }

  object BlogPost {

    def description(content: String): JsValue = {
      // Identify only paragraph here.
      Json.obj()
    }

    def image(data: Option[WPImage]): JsValue = {
      data.map { d =>
        Json.obj(
          "alt" -> d.alt,
          "credits" -> d.credit,
          "origin" -> Json.obj(
            "height" -> d.height,
            "width" -> d.width,
            "url" -> d.url
          ),
          "thumbnails" -> d.thumbnails.map { thumbnail =>
            val key = thumbnail.id match {
              case "thumbnail" => "thumbnail"
              case "medium" => "medium"
              //case "large" =>
              case _ => ""
            }
            Json.obj(
              key -> Json.obj(
                "alt" -> thumbnail.alt,
                "credits" -> thumbnail.credit,
                "origin" -> Json.obj(
                  "height" -> thumbnail.height,
                  "width" -> thumbnail.width,
                  "url" -> thumbnail.url
                )
              )
            )
          }
        )
      } getOrElse JsNull
    }

    def date(at: Date): JsValue = {
      Json.obj()
    }

    def body(content: String): JsValue = {
      val paragraphs = content.split("\n\n").foldRight(Seq.empty[String]) { (p, acc) =>
        ("<p>" + p + "</p>") +: acc
      }

      val blocks = paragraphs.foldLeft(List.empty[Block]) { (blocks, paragraph) =>
        val source = new Source(paragraph)
        wpParagraph2Block(source.getFirstElement) +: blocks
      }

      blocks.zipWithIndex.foldLeft(Json.obj()) {
        case (acc, (block, index)) =>
          Json.obj(index.toString -> Block.toJson(block))
      }
    }

    def author(someone: WPAuthor): JsValue = {
      Json.obj()
    }

    def categories(categories: Seq[WPCategory]): JsValue = {
      Json.arr()
    }

    def apply(wpost: WPost): JsValue =
      Json.obj(
        "uid" -> wpost.slug,
        "title" -> wpost.title,
        "body" -> BlogPost.body(wpost.content),
        "shortlede" -> BlogPost.description(wpost.description),
        "image" -> BlogPost.image(wpost.image),
        "date" -> BlogPost.date(wpost.at),
        "author" -> BlogPost.author(wpost.author),
        "categories" -> BlogPost.categories(wpost.categories),
        "allow_comments" -> wpost.comment
      )
  }

  def main(args: Array[String]) {

    val xml = XML.loadFile("prismicio.wordpress.2015-02-03.xml")
    val items = xml \ "channel" \\ "item"

    val posts = items.filter { item =>
      (item \ "post_type").text == "post" && (item \ "status").text != "trash"
    }

    val images = items.filter { item =>
      (item \ "post_type").text == "attachment" && (item \ "status").text != "trash"
    }.map { image =>
      val id = (image \ "post_id").text
      val url = (image \ "attachment_url").text
      val metadata = (image \\ "postmeta").find { meta =>
        (meta \ "meta_key").text == "_wp_attachment_metadata"
      }.map { meta =>
        (meta \ "meta_value").text
      } getOrElse sys.error("oops")
      val alt = (image \\ "postmeta").find { meta =>
        (meta \ "meta_key").text == "_wp_attachment_image_alt"
      }.map { meta =>
        (meta \ "meta_value").text
      }
      val description = Option((image \ "encoded").filter(_.prefix == "content").text).filter(_ !="")
      val caption = Option((image \ "encoded").filter(_.prefix == "excerpt").text).filter(_ != "")
      WPImage(id, url, alt, description, metadata)
    }

    val authors = (xml \ "channel" \\ "author").flatMap { author =>
      val login = (author \ "author_login").text
      val firstName = (author \ "author_first_name").text
      val lastName = (author \ "author_last_name").text
      Option(WPAuthor(login, firstName, lastName)).filter(!_.fullName.trim.isEmpty)
    }

    posts.map { p =>
      val id = (p \ "post_id").text
      val title = (p \ "title").text
      val content = (p \ "encoded").filter(_.prefix == "content").text
      val author = {
        val login = (p \ "creator").text
        authors.find(_.login == login) getOrElse sys.error("oops")
      }
      val tagsAndCategories = (p \\ "category")
      val categories = tagsAndCategories.filter(_.attribute("domain").exists(x => x.toString == "category")).flatMap(_.attribute("nicename").map(_.toString)).toList
      val tags = tagsAndCategories.filter(_.attribute("domain").exists(x => x.toString == "post_tag")).flatMap(_.attribute("nicename").map(_.toString)).toList
      val date = {
        val datestr = (p \ "post_date_gmt").text
        val format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")
        format.setTimeZone(TimeZone.getTimeZone("GMT"))
        format.parse(datestr)
      }
      val image = {
        val thumbnail = (p \\ "postmeta").find { meta =>
          val key = (meta \ "meta_key").text
          key == "_thumbnail_id"
        }.map(_ \ "meta_value").map(_.text)
        thumbnail.flatMap { id =>
          images.find(_.id == id)
        }
      }
      val commentStatus = (p \\ "comment_status").text == "open"
      val slug = (p \ "post_name").text
      val desc = (p \ "encoded").filter(_.prefix == "excerpt").text
      val post = WPost(id, slug, title, desc, content, categories, tags, author, date, commentStatus, image)
      wp2prismic(post)
    }
  }

  private def wpParagraph2Block(paragraph: Element): Block = {
    val excluded = List("<p>", "</p>")
    paragraph.getNodeIterator.toList.foldLeft(Block()) {
      case (block, tag:Tag) if excluded.contains(tag.tidy) => block
      case (block, tag:StartTag) =>
        block.copy(openTags = (tag, block.text.size) +: block.openTags)
      case (block, tag:EndTag) =>
        val (openTag, start) = block.openTags.head
        val span = Span(start, block.text.size, openTag.tidy, Json.obj())
        block.copy(openTags = block.openTags.tail, spans = span +: block.spans)
      case (block, c:CharacterReference) => block append c.getChar
      case (block, text) => block append text.toString
    }
  }

  private def wp2prismic(wpost: WPost) = {
    BlogPost(wpost)
  }
}
