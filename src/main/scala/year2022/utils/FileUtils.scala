package year2022.utils

import cats.effect.{IO, Resource}
import java.io._

object FileUtils {

  def inputStream(f: File): Resource[IO, FileInputStream] =
    Resource.make {
      IO.blocking(new FileInputStream(f))
    } { inStream =>
      IO.blocking(inStream.close()).handleErrorWith(_ => IO.unit)
    }

  def readLine(path: String): IO[Seq[String]] = {
    inputStream(new File(path))
      .map(is => new BufferedReader(new InputStreamReader(is)))
      .use(bufferedReader => {
        def getValue(): Seq[String] = if (bufferedReader.ready()) Seq(bufferedReader.readLine()) ++ getValue() else Nil
        IO(getValue())
      })
  }

}
