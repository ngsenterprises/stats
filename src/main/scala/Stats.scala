/**
 * Created by woody on 4/22/2016.
 */

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.math.Ordering
import scala.util.{Failure, Success, Try}
import scala.collection.immutable.{TreeMap, List, Vector}
import scala.collection.mutable._


object MyApp extends App {

  object Median {

    case class DataNode(isSorted: Boolean, data: Array[Int])

    import scala.collection.mutable._

    def partitionOnEq(k: Int, data: Array[Int]): ArrayBuffer[DataNode] = {
      data match {
        case v if (v.isEmpty) => new ArrayBuffer[DataNode]
        case v if (!v.isEmpty) => {
          val res = data.foldLeft((ArrayBuffer.empty[Int], k +: ArrayBuffer.empty[Int], ArrayBuffer.empty[Int])) {
            case ((acl:ArrayBuffer[Int], ace:ArrayBuffer[Int], acg:ArrayBuffer[Int]), i) => {
              if (i == k)       ace.append(i)
              else if (i < k)   acl.append(i)
              else              acg.append(i)
              (acl, ace, acg)
            }}
          var buf = ArrayBuffer.empty[DataNode]
          if (!res._1.isEmpty)
            buf.append(new DataNode(res._1.length < 2, res._1.toArray))
          if (!res._2.isEmpty)
            buf.append(new DataNode(true, res._2.toArray))
          if (!res._3.isEmpty)
            buf.append(new DataNode(res._3.length < 2, res._3.toArray))
          buf
        }
      }
    }

    def getMedianIndexNode(medianIndex:Int, nodes:ArrayBuffer[DataNode]): Int = {

      val datalen = nodes.length
      if (datalen <= 0) { println("getMedianNodeIndex ERROR: no data nodes."); -1 } else {
        var aggLen = 0//nodes(0).data.length
        var index = 0
        while (index < datalen && aggLen -1 < medianIndex) {
          val nodelen = nodes(index).data.length
          aggLen += nodelen
          index += 1
        }
        index -1
      }
    }

    def median(init: DataNode): Int = {

      var err = 0

      val MedianValues = new ArrayBuffer[Int]
      val MedianIndexes = new ArrayBuffer[Int]
      MedianIndexes += init.data.length/2
      if (init.data.length%2 == 0) MedianIndexes.prepend(init.data.length/2 -1)

      var nodes = new ArrayBuffer[DataNode]
      nodes += init

      while (err == 0 && !MedianIndexes.isEmpty) {

        val nodeIndex = getMedianIndexNode(MedianIndexes.head, nodes)
        if (nodeIndex < 0) err = -1 else {

          var nodesSplit = nodes.splitAt(nodeIndex)

          if (nodesSplit._2.isEmpty) err = -2 else {
            val pNode = nodesSplit._2.head
            val pUpper = nodesSplit._2.tail

            if (pNode.isSorted) {

              val nodeSubIndex = nodesSplit._1.foldLeft(0)((ac, nd) => ac +nd.data.length)

              val subindex = MedianIndexes.head -nodeSubIndex

              MedianValues += pNode.data(subindex)

              MedianIndexes.remove(0)
            }
            else {
              val parts = partitionOnEq(pNode.data.head, pNode.data.tail)
              nodes = nodesSplit._1 ++ parts ++ pUpper
            }
          }
        }
      }
      println
      println("results: ")
      println(MedianValues.toList)
      //println("nodes:")
      //nodes.foreach(nd => println(nd.isSorted  +" : " +nd.data.toList))
      err
    }

    def median2(data: Seq[Int]): Int = {
      val (lower, upper) = data.sortWith(_ < _).splitAt(data.size/2)
      if (data.size % 2 == 0) (lower.last +upper.head)/2 else upper.head
    }

    val rndData = collection.mutable.ArrayBuffer.fill(5000000)(scala.util.Random.nextInt(100))

    median(new DataNode(false, rndData.toArray))

  }

  //Median



  object MedianRecurse {
    import scala.collection.mutable._

    case class DataNode(isSorted: Boolean, var data: Array[Int])
    val max_data_val = 100



    def partitionOnEq(k: Int, data: Array[Int]): ArrayBuffer[DataNode] = {
      type abi = ArrayBuffer[Int]
      data match {
        case v if (v.isEmpty) => ArrayBuffer.empty[DataNode]
        case v => {
          val res = data.foldLeft((new abi, new abi, new abi)) {
            case ((acl:abi, ace:abi, acg:abi), i) =>
              if (i == k)       (acl, ace :+ i, acg)
              else if (i < k)   (acl :+ i, ace, acg)
              else              (acl, ace, acg :+ i)
          }
          val ret = ArrayBuffer.empty[DataNode]
          if (0 < res._1.length)
            ret.append(new DataNode(res._1.length == 1, res._1.toArray))
          if (0 < res._2.length)
            ret.append(new DataNode(true, res._2.toArray))
          if (0 < res._3.length)
            ret.append(new DataNode(res._3.length == 1, res._3.toArray))
          ret
        }
      }
    }

    def getNodeIndex(offset:Int, nodes:ArrayBuffer[DataNode]): Int = {
      var indexCpy = offset
      val datalen = nodes.length
      if (datalen <= 0) { println("getMedianNodeIndex ERROR: no data nodes."); -1 } else {
        var index = 0
        while (index < datalen && nodes(index).data.length <= indexCpy) {
          indexCpy -= nodes(index).data.length
          index += 1
        }
        index
      }
    }

    def medianInit(lb:Int, ub:Int, initNode: DataNode): Int = {

      val medList = initNode.data.length/2 :: (if (initNode.data.length % 2 == 0) initNode.data.length/2 -1 :: Nil else Nil)

      def recurse(medianOffsets:List[Int], node: DataNode): Int  = {

        println
        println("recurse:")
        println("medianOffsets: " +medianOffsets)
        println("data: " +node.isSorted +", " +node.data.toList)

        //SORTED
        if (node.isSorted) {
          //println("sorted:")
          //medianOffsets.foreach(offset => println(offset.toString))

          //val sum = medianOffsets.foldLeft(0)((ac, i) => {println(s"node.data(${i}): ${node.data(i)}"); ac +node.data(i)});println("sum: " +sum.toString)
          val sum = medianOffsets.foldLeft(0)((ac, i) => ac +node.data(i))
          //medianOffsets.foldLeft(0)((ac, i) => ac + node.data(i)) / medianOffsets.length
          //println("median: " +(sum/medianOffsets.length))
          //sum/medianOffsets.length
          sum/medianOffsets.length
        }
        //UNSORTED
        else {
          val medium = MedianRecurse.max_data_val/2
          val pivot = (0 until node.data.length/3).foldLeft(node.data(0))((ac, i) => if (node.data(i) -medium <= ac -medium) node.data(i) else ac);println("pivot: " +pivot)
          val parts = partitionOnEq(pivot, node.data);println("parts: ");parts.foreach(dn => println(dn.data.toList))

          if (medianOffsets.length == 1) {
            val nodeIndex = getNodeIndex(medianOffsets(0), parts);//println("1 nodeIndex: " +nodeIndex.toString)
            val dif = (0 until nodeIndex).foldLeft(0)((ac, index) =>  ac +parts(index).data.length);//println("dif: " +dif)
            val ofs = medianOffsets.map(offset => offset -dif);//println("ofs: " +ofs)

            recurse(ofs, parts(nodeIndex))
          }
          else if (medianOffsets.length == 2) {
            val nodeIndex0 = getNodeIndex(medianOffsets(0), parts);//println("2 nodeIndex0: " +nodeIndex0.toString)
            val dif0 = (0 until nodeIndex0).foldLeft(0)((ac, index) =>  ac +parts(index).data.length);//println("dif0: " +dif0)
            val ofs0 = medianOffsets(0) -dif0;//println("ofs0: " +ofs0)
            val nodeIndex1 = getNodeIndex(medianOffsets(1), parts);//println("2 nodeIndex1: " +nodeIndex1)

            if (nodeIndex0 == nodeIndex1) {
              val ret = recurse(List(ofs0), parts(nodeIndex0))
              println("ret: " +ret.toString)
              ret
            }
            else {
              val dif1 = (0 until nodeIndex1).foldLeft(0)((ac, index) =>  ac +parts(index).data.length);//println("dif1: " +dif1)
              val ofs1 = medianOffsets(1) -dif1;//println("ofs1: " +ofs1)

              val r0 = recurse(List(ofs0), parts(nodeIndex0))
              val r1 = recurse(List(ofs1), parts(nodeIndex1))
              println(s"(${r0},${r1})")

              (r0 +r1)/2
            }
          }
          else {
            println(s"ERROR: median index length ${medianOffsets.length}.")
            -3
          }
        }
      }

      recurse(medList, initNode)
    }

    val data_size = 100
    val lower_bound = 0
    val upper_bound = 100
    val rndData = collection.mutable.ArrayBuffer.fill(data_size)(scala.util.Random.nextInt(upper_bound))

    medianInit(new DataNode(false, rndData.toArray))


    /*
    val srt = rndData.sorted
    if (srt.length%2 == 0)
      println("median = (" +srt(srt.length/2).toString +")")
    else {
      val v1 = srt(srt.length/2)
      val v2 = srt(srt.length/2 +1)
      println(s"median = (${v1.toString}, ${v2.toString}, ${(v1 +v2)/2} ")
    }
    */



  }

  MedianRecurse










}
