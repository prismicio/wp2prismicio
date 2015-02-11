package wp2prismic

import scala.collection.JavaConversions._
import scala.xml.XML
import net.htmlparser.jericho._
import org.lorecraft.phparser.SerializedPhpParser
import java.util.{ Date, TimeZone }
import java.text.SimpleDateFormat
import scala.collection.mutable.{ Map => MMap }
import play.api.libs.json.{ JsValue, Json, JsArray, JsNull, JsString }
import java.net.URL
import java.io.{ File, FileOutputStream }

object Wp2Prismic {

  // WORDPRESS

  case class WPost(id: String, slug: String, title: String, description: String, content: String, categories: List[WPCategory], tags: List[String], author: WPAuthor, at: Date, comment: Boolean, image: Option[WPImage])

  type WParagraph = String

  case class WPImage(id: String, url: String, width: Option[Int], height: Option[Int], credit: Option[String], copyright: Option[String], alt: Option[String], thumbnails: Seq[WPImage])

  case class WPAuthor(login: String, firstName: String, lastName: String) {
    lazy val fullname = firstName + " " + lastName
  }

  case class WPCategory(slug: String, name: String)

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
      val credit = meta.flatMap(m => m.get("credit").map(_.asInstanceOf[String])).filter(!_.isEmpty)
      val copyright = meta.flatMap(m => m.get("copyright").map(_.asInstanceOf[String])).filter(!_.isEmpty)
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
  case class Block(typ: String, text: String, spans: List[Span], data: JsValue, openTags: List[(Tag, Int)]) {
    def append(t: String): Block = Block(typ, text + t, spans, JsNull, openTags)
    def append(c: Char): Block = Block(typ, text + c, spans, JsNull, openTags)
  }

  object Block {

    def apply(typ: String): Block = Block(typ, "", Nil, JsNull, Nil)

    def listItem(text: String) =
      Block("list-item", text, Nil, JsNull, Nil)

    def olistItem(text: String) =
      Block("o-list-item", text, Nil, JsNull, Nil)

    def paragraph = apply("paragraph")

    def preformatted(text: String) =
      Block("preformatted", text, Nil, JsNull, Nil)

    def image(image: WPImage, linkTo: Option[String]) = {
      val link = linkTo.map(url => Json.obj("linkTo" -> Json.obj("url" -> url))).getOrElse(Json.obj())
      val data = Json.obj(
        "alt" -> image.alt,
        "credits" -> image.credit,
        "origin" -> Json.obj(
          "height" -> image.height,
          "width" -> image.width,
          "url" -> image.url
        ),
        "thumbnails" -> image.thumbnails.map { thumbnail =>
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
      ) ++ link
      Block("image", text="", spans=Nil, openTags=Nil, data=data)
    }

    def toJson(block: Block): JsValue =
      Json.obj(
        "type" -> block.typ,
        "content" -> Json.obj(
          "spans" -> block.spans.map(Span.toJson),
          "text" -> block.text
        ),
        "data" -> block.data
      )

    def toJsonSeq(blocks: Seq[Block]): JsValue =
      JsArray(blocks.map(Block.toJson))
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
      Span(start, end, "label", JsString(data))

    def quote(start: Int, end: Int): Span =
      label(start, end, "block-quotation")
  }


  object Category {

    def exportTo(id: String, content: JsValue): File = {
      writeToFile(ref(id), content)
    }

    def ref(slug: String) = s"category-${slug}"

    def apply(slug: String, name: String) =
      Json.obj(
        "tags" -> Json.arr(),
        "type" -> "category",
        "uid" -> slug,
        "name" -> name
      )
  }

  object Author {

    def exportTo(id: String, content: JsValue): File = {
      writeToFile(ref(id), content)
    }

    def ref(login: String) = s"author-${login}"

    def apply(fullname: String) =
      Json.obj(
        "tags" -> Json.arr(),
        "type" -> "author",
        "full_name" -> fullname
      )
  }

  object BlogPost {

    def ref(id: String) = s"post-${id}"

    def exportTo(id: String, content: JsValue): File = {
      writeToFile(ref(id), content)
    }

    def description(content: String, images: List[WPImage]): JsValue =
      Block.toJsonSeq(wpContent2Blocks(content, images))

    def image(data: Option[WPImage]): JsValue = {
      data.map { d =>
        Block.image(d, None).data
      } getOrElse JsNull
    }

    def date(at: Date): JsValue = {
      val formatter = new SimpleDateFormat("yyyy-MM-dd")
      JsString(formatter.format(at))
    }

    def body(content: String, images: List[WPImage]): JsValue =
      Block.toJsonSeq(wpContent2Blocks(content, images))

    def author(someone: WPAuthor): JsValue =
      Json.obj(
        "mask" -> "author",
        "id" -> Author.ref(someone.login)
      )

    def categories(categories: List[WPCategory]): JsValue = {
      JsArray(categories.map { category =>
        Json.obj(
          "id" -> Category.ref(category.slug),
          "mask" -> "category"
        )
      })
    }

    def apply(wpost: WPost, images: List[WPImage]): JsValue =
      Json.obj(
        "tags" -> wpost.tags,
        "type" -> "post",
        "uid" -> wpost.slug,
        "title" -> wpost.title,
        "body" -> BlogPost.body(wpost.content, images),
        "shortlede" -> BlogPost.description(wpost.description, images),
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
      val description = Option((image \ "encoded").filter(_.prefix == "content").text).filter(!_.isEmpty)
      val caption = Option((image \ "encoded").filter(_.prefix == "excerpt").text).filter(!_.isEmpty)
      WPImage(id, url, alt, description, metadata)
    }

    val authors = (xml \ "channel" \\ "author").flatMap { author =>
      val login = (author \ "author_login").text
      val firstName = (author \ "author_first_name").text
      val lastName = (author \ "author_last_name").text
      Option(WPAuthor(login, firstName, lastName)).filter(!_.fullname.trim.isEmpty)
    }.toList

    val categories = (xml \ "channel" \\ "category").flatMap { category =>
      val slug = (category \ "category_nicename").text
      val name = (category \ "cat_name").text
      Option(WPCategory(slug, name)).filter(!_.slug.isEmpty)
    }

    val blogPostDocs = posts.map { p =>
      val id = (p \ "post_id").text
      val title = (p \ "title").text
      val content = (p \ "encoded").filter(_.prefix == "content").text
      val author = {
        val login = (p \ "creator").text
        authors.find(_.login == login) getOrElse sys.error("oops")
      }
      val categories = {
        (p \\ "category").filter(_.attribute("domain").exists(x => x.toString == "category")).flatMap { category =>
          category.attribute("nicename").map(_.toString).map { slug =>
            WPCategory(slug, category.text)
          }
        }.toList
      }
      val tags = (p \\ "category").filter(_.attribute("domain").exists(x => x.toString == "post_tag")).flatMap(_.attribute("nicename").map(_.toString)).toList
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

      id -> BlogPost(post, images.toList)
    }

    val authorFiles = authors.map(a => Author.exportTo(a.login, Author(a.fullname)))
    val categoryFiles = categories.map(c => Category.exportTo(c.slug, Category(c.slug, c.name)))
    val blogPostFiles = blogPostDocs.map {
      case (id, blogPost) => BlogPost.exportTo(id, blogPost)
    }
    val filesToZip = authorFiles ++: categoryFiles ++: blogPostFiles
    ZipUtils.zip(filesToZip, new File("export/wp.zip"))
  }

  private def writeToFile(name: String, content: JsValue): File = {
    val dataToWrite = Json.prettyPrint(content).getBytes("utf-8")
    val file = new File("export/" + name + ".json")
    val out = new FileOutputStream(file)
    out.write(dataToWrite)
    out.close()
    file
  }

  private def wpList2Block(el: Element): Seq[Block] = {
    el.getChildElements.map { li =>
      Block.listItem(li.getContent.toString)
    }
  }

  private def wpOrderedList2Block(el: Element): Seq[Block] = {
    el.getChildElements.map { li =>
      Block.olistItem(li.getContent.toString)
    }
  }

  private def wpCode2Block(el: Element): Block = {
    Block.preformatted(el.getContent.toString)
  }

  private def wpLink2Block(el: Element, images: List[WPImage]): Block = {
    val url = el.getAttributeValue("href")
    val image = el.getChildElements.find { child =>
      child.getStartTag.getName == "img"
    }
    image.map { img =>
      wpImage2Block(img, Some(url), images)
    } getOrElse {
      wpParagraph2Block(el)
    }
  }

  private def wpImage2Block(el: Element, linkTo: Option[String], images: List[WPImage]): Block = {
    val image = images.find(_.url == el.getAttributeValue("src")) getOrElse sys.error("oops")
    Block.image(image, linkTo)
  }

  private def wpParagraph2Block(el: Element): Block = {
    val excluded = List("p", "ul", "ol", "img", "code")
    el.getNodeIterator.toList.foldLeft(Block.paragraph) {
      case (block, tag:Tag) if excluded.contains(tag.getName) => block
      case (block, tag:StartTag) =>
        block.copy(openTags = (tag, block.text.size) +: block.openTags)
      case (block, tag:EndTag) =>
        val (openTag, start) = block.openTags.head
        val end = block.text.size
        val span = openTag.getName match {
          case "strong" => Span.strong(start, end)
          case "em" => Span.em(start, end)
          case "a" =>
            val url = openTag.getElement.getAttributeValue("href")
            Span.hyperlink(start, end, url)
          case "blockquote" =>
            println("blockquote")
            Span.quote(start, end)
          case l => Span.label(start, end, l)
        }
        block.copy(openTags = block.openTags.tail, spans = span +: block.spans)
      case (block, c:CharacterReference) => block append c.getChar
      case (block, text) => block append text.toString
    }
  }

  private def nextJustChildElement(el: Element): Option[Element] = {
    val it = el.getNodeIterator()
    it.next()
    if(it.hasNext) {
      it.next() match {
        case t: Tag => Some(t.getElement)
        case _ => None
      }
    } else None
  }

  private def isParagraph(el: Element): Boolean =
    el.getStartTag.getName == "p"

  private def wpContent2Blocks(content: String, images: List[WPImage]): List[Block] = {
    val normalized = content.split("\n\n").foldLeft("") { (acc, p) =>
      acc ++ ("<p>" + p + "</p>")
    }
    val source = new Source(normalized)
    val parsed = source.getChildElements.foldLeft(Seq.empty[Block]) { (acc, el) =>
      val knownBlocks = List("p", "ul", "ol", "code", "a", "img")
      //FLATTEN PARAGRAPH
      val flatten = if(isParagraph(el)) {
        val innerBlock = nextJustChildElement(el).filter(x => knownBlocks.exists(_ == x.getStartTag.getName))
        innerBlock getOrElse el
      } else el
      flatten.getStartTag.getName match {
        case "p" | "blockquote" => acc :+ wpParagraph2Block(flatten)
        case "ul" => acc ++: wpList2Block(flatten)
        case "ol" => acc ++: wpOrderedList2Block(flatten)
        case "code" =>acc :+ wpCode2Block(flatten)
        case "a" => acc :+ wpLink2Block(flatten, images)
        case "img" => acc :+ wpImage2Block(flatten, None, images)
        case _ => acc
      }
    }.toList

    parsed.collect {
      case b@Block("paragraph", text, _, _, _) if(!text.trim.isEmpty) => b
      case x => x
    }
  }
}
