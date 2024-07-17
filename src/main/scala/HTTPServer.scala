package HTTP.Server

import parsley.*
import parsley.combinator.*
import parsley.character.*
import parsley.Parsley
import parsley.extension.HaskellStyleMap
import cats.*
import cats.implicits.*
import cats.effect.*

import java.net.ServerSocket

val request =
  """GET / HTTP/1.1
Host: google.com
User-Agent: curl/8.8.0
Accept: application/json, */*"""

sealed trait Mode
case object GET extends Mode

object Mode:
  def apply(modeStr: String): Mode = modeStr match
    case "GET"    => GET
    case "POST"   => GET
    case "PUT"    => GET
    case "DELETE" => GET

sealed trait StatusCode
case class Ok(code: Int) extends StatusCode

case class Request(
    mode: Mode,
    path: String,
    version: String,
    headers: Map[String, String]
)

case class Response(version: String, status: StatusCode, body: String)

object Response:
  given Show[Response] with
    def show(r: Response): String = r.status match
      case Ok(code) => s"${r.version} $code OK\r\r${r.body}"

lazy val p: Parsley[Request] = for {
  mode <- pMode
  _ <- space
  path <- pPath
  _ <- space
  version <- pVersion
  _ <- newline
  headers <- pHeaders
} yield Request(mode, path, version, headers)

lazy val pMode: Parsley[Mode] =
  string("GET") <|>
    string("POST") <|>
    string("PUT") <|>
    string("DELETE") map
    Mode.apply

lazy val pPath: Parsley[String] = stringOfMany(noneOf(' '))
lazy val pVersion: Parsley[String] =
  string("HTTP") *> char('/') *> stringOfMany(noneOf('\n'))
lazy val pHeaders: Parsley[Map[String, String]] =
  sepBy(pHeader, newline) <* eof map Map.from
lazy val pHeader: Parsley[(String, String)] =
  stringOfMany(noneOf(':')) >>= ((key: String) =>
    string(": ") *> stringOfMany(noneOf('\n')) map ((key, _))
  )

val r = p.parse(request).getOrElse(Request(GET, "/", "", Map()))

def loadFile(path: String): String =
  val fileName = if path.endsWith("/") then "index.html" else path
  scala.io.Source.fromFile(fileName).mkString

def createResponse(req: Request): Response =
  val version = "HTTP/" ++ req.version
  val body = loadFile(req.path)
  Response(version, Ok(200), body)

// serveRequest(r).show

def errorResponse(e: String): Response =
  Response("HTTP/1.1", Ok(200), e)

def serveRequest(server: ServerSocket): IO[Unit] = for
  soc <- IO.blocking(server.accept)
  _ <- IO.println("connection accepted")
  out = soc.getOutputStream
  in = soc.getInputStream
  in <- IO.blocking(in.readAllBytes).map(_.mkString)
  _ <- IO.println(s"input: $in")
  req = p.parse(in)
  _ <- IO.println(s"request parsed: $req")
  resp = req.fold((e => errorResponse(e)), (req => createResponse(req)))
  _ <- IO.println(s"response created: $resp")
  _ <- IO.blocking(out.write(resp.show.getBytes))
yield ()

object App extends IOApp.Simple:
  def run: IO[Unit] =
    Resource.make(IO(new ServerSocket(8980)))(s => IO(s.close))
      .use(serveRequest)
