package pounce.inspections.effects

/**
 * Known blocking APIs, as data. Matching is on the class that declares the resolved method (or the constructed
 * class for `new X(...)`), optionally including subclasses / implementations.
 *
 * @param owner         fully qualified class or object name (objects without `$`)
 * @param members       method names; empty = every method; `<init>` = constructor
 * @param subclasses    also match members inherited by / overridden in subclasses
 * @param interruptible the call reacts to `Thread.interrupt`, so `IO.interruptible` is a good fit
 * @param label         short name used in the message
 */
final case class BlockingApi(owner: String, members: Set[String], subclasses: Boolean, interruptible: Boolean, label: String)

object BlockingCalls {

  private def api(owner: String, members: String*)(label: String, subclasses: Boolean = false, interruptible: Boolean = false) =
    BlockingApi(owner, members.toSet, subclasses, interruptible, label)

  val all: Seq[BlockingApi] = Seq(
    // threads and synchronisation
    api("java.lang.Thread", "sleep", "join")("Thread.sleep/join", interruptible = true),
    api("java.lang.Object", "wait")("Object.wait", interruptible = true),
    api("java.lang.Process", "waitFor")("Process.waitFor", subclasses = true, interruptible = true),
    api("java.util.concurrent.Future", "get")("Future.get", subclasses = true, interruptible = true),
    api("java.util.concurrent.CompletableFuture", "join", "get")("CompletableFuture.join/get"),
    api("java.util.concurrent.CountDownLatch", "await")("CountDownLatch.await", interruptible = true),
    api("java.util.concurrent.CyclicBarrier", "await")("CyclicBarrier.await", interruptible = true),
    api("java.util.concurrent.Semaphore", "acquire")("Semaphore.acquire", interruptible = true),
    api("java.util.concurrent.BlockingQueue", "take", "put")("BlockingQueue.take/put", subclasses = true, interruptible = true),
    api("java.util.concurrent.locks.Lock", "lock", "lockInterruptibly")("Lock.lock", subclasses = true),
    api("scala.concurrent.Await", "result", "ready")("Await.result"),
    // java.io
    api("java.io.InputStream", "read", "readAllBytes", "readNBytes", "transferTo", "skip")("InputStream.read", subclasses = true),
    api("java.io.OutputStream", "write", "flush")("OutputStream.write", subclasses = true),
    api("java.io.Reader", "read", "skip", "transferTo")("Reader.read", subclasses = true),
    api("java.io.BufferedReader", "readLine", "lines")("BufferedReader.readLine", subclasses = true),
    api("java.io.Writer", "write", "flush", "append")("Writer.write", subclasses = true),
    api("java.io.FileInputStream", "<init>")("new FileInputStream"),
    api("java.io.FileOutputStream", "<init>")("new FileOutputStream"),
    api("java.io.FileReader", "<init>")("new FileReader"),
    api("java.io.FileWriter", "<init>")("new FileWriter"),
    api("java.io.RandomAccessFile", "<init>", "read", "write", "seek", "readFully")("RandomAccessFile"),
    api("java.io.File", "createNewFile", "delete", "exists", "length", "list", "listFiles", "mkdir", "mkdirs", "renameTo",
      "isDirectory", "isFile", "lastModified", "getCanonicalPath", "createTempFile")("java.io.File"),
    // java.nio
    api("java.nio.file.Files")("java.nio.file.Files"),
    api("java.nio.channels.FileChannel", "open", "read", "write", "force", "transferTo", "transferFrom")("FileChannel", subclasses = true),
    // JDBC
    api("java.sql.DriverManager", "getConnection")("DriverManager.getConnection"),
    api("javax.sql.DataSource", "getConnection")("DataSource.getConnection", subclasses = true),
    api("java.sql.Connection", "prepareStatement", "createStatement", "prepareCall", "commit", "rollback", "close", "setAutoCommit")(
      "JDBC Connection",
      subclasses = true
    ),
    api("java.sql.Statement", "execute", "executeQuery", "executeUpdate", "executeBatch", "executeLargeUpdate")("JDBC Statement", subclasses = true),
    api("java.sql.ResultSet", "next")("ResultSet.next", subclasses = true),
    // scala.io
    api("scala.io.Source", "fromFile", "fromURL", "fromURI", "fromInputStream", "fromResource", "getLines")("scala.io.Source", subclasses = true),
    // network
    api("java.net.URL", "openStream", "getContent")("URL.openStream"),
    api("java.net.URLConnection", "connect", "getInputStream", "getOutputStream", "getContent", "getResponseCode")("URLConnection", subclasses = true),
    api("java.net.Socket", "<init>", "connect")("Socket"),
    api("java.net.ServerSocket", "accept")("ServerSocket.accept"),
    api("java.net.InetAddress", "getByName", "getAllByName", "getLocalHost")("InetAddress (DNS lookup)"),
    api("java.net.http.HttpClient", "send")("HttpClient.send", subclasses = true),
    api("scala.sys.process.ProcessBuilder", "!", "!!", "lazyLines")("sys.process", subclasses = true)
  )

  /** In-memory implementations of the I/O base classes: never blocking. */
  val excludedOwners: Set[String] = Set(
    "java.io.ByteArrayInputStream",
    "java.io.ByteArrayOutputStream",
    "java.io.StringReader",
    "java.io.StringWriter",
    "java.io.CharArrayReader",
    "java.io.CharArrayWriter",
    "java.io.PrintStream" // System.out / Console: treated as non-blocking, like the IO.println convention
  )
}
