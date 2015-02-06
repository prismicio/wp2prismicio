import scala.collection.JavaConversions._
import scala.xml.XML
import org.jsoup._
import org.jsoup.nodes.Element
import org.lorecraft.phparser.SerializedPhpParser
import java.util.{ Date, TimeZone }
import java.text.SimpleDateFormat
import scala.collection.mutable.{ Map => MMap }
import play.api.libs.json.{ JsValue, Json, JsArray }

object Wp2Prismic {

  // WORDPRESS

  case class WPost(id: String, slug: String, title: String, description: String, content: String, categories: List[String], tags: List[String], author: String, at: Date, comment: Boolean, image: Option[WPImage])

  type WParagraph = String

  case class WPImage(id: String, url: String, width: Option[Int], height: Option[Int], credit: Option[String], copyright: Option[String], alt: Option[String], thumnails: Seq[WPImage])

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
            WPImage(id + "_" + file, thumbnailUrl, width, height, credit, copyright, alt, Seq.empty)
          }
        }
      }
      WPImage(id, url, width, height, credit, copyright, alt, thumbnails)
    }
  }

  case class WPAuthor(login: String, firstName: String, lastName: String) {
    lazy val fullName = firstName + " " + lastName
  }

  // PRISMIC
  object StructuredText {

    def apply(text: String, spans: JsArray): JsValue =
      Json.obj(
        "type" -> "paragraph",
        "content" -> Json.obj(
          "text" -> text,
          "spans" -> spans
        )
      )

    def strong(start: Int, end: Int) =
      Json.obj("start" -> start, "end" -> end, "type" -> "strong")

    def em(start: Int, end: Int) =
      Json.obj("start" -> start, "end" -> end, "type" -> "strong")

    def hyperlink(start: Int, end: Int, url: String) =
      Json.obj(
        "start" -> start,
        "end" -> end,
        "type" -> "hyperlink",
        "data" -> Json.obj("url" -> url)
      )

    def label(start: Int, end: Int, data: String) =
      Json.obj(
        "start" -> start,
        "end" -> end,
        "type" -> "label",
        "data" -> data
      )

    def quote(start: Int, end: Int) =
      label(start, end, "quote")

    def del(start: Int, end: Int) =
      label(start, end, "del")

    def ins(start: Int, end: Int) =
      label(start, end, "ins")

    def img(start: Int, end: Int, width: Int, height: Int, url: String) =
     Json.obj(
        "start" -> start,
        "end" -> end,
        "type" -> "image",
        "data" -> Json.obj(
          "origin" -> Json.obj(
            "height" -> height,
            "width" -> width,
            "url" -> url
          )
        )
      )
  }

  object BlogPost {

    def apply(slug: String, title: String, body: JsValue, shortlede: JsValue, image: JsValue, author: JsValue, date: JsValue, categories: Seq[JsValue], comment: Boolean): JsValue =
      Json.obj(
        "uid" -> slug,
        "title" -> title,
        "body" -> body,
        "shortlede" -> shortlede,
        "image" -> image,
        "date" -> date,
        "author" -> author,
        "categories" -> categories.zipWithIndex.foldLeft(Json.obj()) {
          case (acc, (category, index)) =>
            acc ++ Json.obj(index.toString -> category)
        },
        "allow_comments" -> comment
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
        authors.find(_.login == login) map (_.fullName) getOrElse login
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

  private def wp2prismic(wpost: WPost) = {
    val content = wpost.content.split("\n\n").foldLeft("") { (acc, p) =>
      acc + "<p>" + p + "</p>"
    }

    val root = Jsoup.parse(content).body

    def step(el: Element, cursor: Int, level: Int): JsValue = {
      root.childNodes.foldLeft(Json.obj()) { (acc, el) => //PARAGRAPH LEVEL
        //println("------------>")
        //println(el.acc)
        acc
      }
    }

    step(root, 0, 0)
  }
}
