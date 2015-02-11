package wp2prismic;

import java.io.*;
import java.net.URI;
import java.util.*;
import java.util.zip.*;

public class ZipUtils {

  public static void zip(Iterable<File> files, File zipfile) throws IOException {
    OutputStream out = new FileOutputStream(zipfile);
    Closeable res = out;
    try {
      ZipOutputStream zout = new ZipOutputStream(out);
      res = zout;
      for (File kid : files) {
          java.lang.String name = kid.getName();
          if (kid.isDirectory()) {
              throw new RuntimeException("Directories not supported in zipping a list of files");
          } else {
              zout.putNextEntry(new ZipEntry(name));
              copy(kid, zout);
              zout.closeEntry();
          }
      }
    }
    finally {
      res.close();
    }
  }

  private static void copy(InputStream in, OutputStream out) throws IOException {
    byte[] buffer = new byte[1024];
    while (true) {
      int readCount = in.read(buffer);
      if (readCount < 0) {
        break;
      }
      out.write(buffer, 0, readCount);
    }
  }

  private static void copy(File file, OutputStream out) throws IOException {
    InputStream in = new FileInputStream(file);
    try {
      copy(in, out);
    } finally {
      in.close();
    }
  }

  private static void copy(InputStream in, File file) throws IOException {
    OutputStream out = new FileOutputStream(file);
    try {
      copy(in, out);
    } finally {
      out.close();
    }
  }
}
