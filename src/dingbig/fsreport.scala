package dingbig

import java.io.File

import scala.io.Source

/**
  * Created by d on 16/10/2017.
  */
object fsreport extends App {

  var curDir = new File(System.getProperty("user.dir"))


  val regex_cd = "cd (.*)".r
  val regex_ls = "ls".r

  val src = Source.fromInputStream(System.in, "utf-8")
  val inputs = src.getLines()

  printf("%s>",curDir)
  inputs.foreach(processInput)

  def totalSize(f: File): Long = {
    if (f.isFile) f.length()
    else {
      val subs = f.listFiles()

      if(subs == null) 0
      else subs.foldLeft(0L)((z, x) => z + totalSize(x))
    }
  }

  def readableSize(f: File): String = {
    val sz = totalSize(f)
    if(sz /1024 /1024 /1024 > 0) "%.3f G".format(sz/1024f/1024f/1024f)
    else if(sz /1024 /1024 > 0) "%.3f M".format(sz/1024f/1024f)
    else if(sz /1024 > 0) "%.3f K".format(sz/1024f)
    else "%d B".format(sz)
  }

  def findFiles(f: File): List[File] = {
    if(f.isFile) List(f)
    else {
      val subs = f.listFiles()
      if(subs == null) List(f)
      else  f :: subs.flatMap(findFiles(_)).toList
    }
  }





  def processInput(input: String): Unit = {
    input match {
      case "quit" =>
        System.exit(-1)
      case "exit" =>
        System.exit(-1)
      case regex_cd(dst) =>
        dst.split(File.separatorChar).foreach {
          nxt =>
            nxt match {
              case ".." => curDir = curDir.getParentFile
              case "." =>
              case _ => curDir = new File(curDir, nxt)
            }
        }
      case "sizes" =>
        println("sorting...")
        findFiles(curDir).filter(_.isFile).sortWith((f1, f2) => f1.length() < f2.length()).foreach {
          f =>printf("%-96s %s\n", f, readableSize(f))
        }
      case regex_ls() =>
        curDir.listFiles().filter(_.isDirectory).foreach(f=>printf("%-96s %s\n", f.getName, readableSize(f)))
        curDir.listFiles().filter(_.isFile).foreach(f=>printf("%-96s %s\n",  f.getName, readableSize(f)))
      case _ =>
        println("bad input " + input)
    }
    printf("%s>", curDir.getAbsolutePath)
  }
}
