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
    import scala.collection.mutable._

    def partitionOnEq(k: Int, data: Array[Int]): (Array[Int], Array[Int], Array[Int]) = {
      data match {
        case v if (v.isEmpty) => (Array.empty[Int], Array.empty[Int], Array.empty[Int])
        case v => {
          val res = data.foldLeft((ArrayBuffer.empty[Int], ArrayBuffer.empty[Int], ArrayBuffer.empty[Int])) {
            case ((acl:ArrayBuffer[Int], ace:ArrayBuffer[Int], acg:ArrayBuffer[Int]), i) => {
              if (i == k)       (acl, ace :+ i, acg)
              else if (i < k)   (acl :+ i, ace, acg)
              else              (acl, ace, acg :+ i)
            }}
          ( res._1.toArray, res._2.toArray, res._3.toArray )
        }
      }
    }

    def median(initData: Array[Int]): Int = {
      //println(s"initData: ${initData.toList}")
      var err = 0
      var pivotLB = Int.MinValue
      var pivotUB = Int.MaxValue
      val MedianValues = new ArrayBuffer[Int]
      var MedianIndexes = new ArrayBuffer[Int]
      MedianIndexes += initData.length/2
      if (initData.length%2 == 0) MedianIndexes.prepend(initData.length/2 -1)
      //println(s"MedianIndexes: ${MedianIndexes.toList}")

      var pivotCurr = initData.head
      //println(s"pivotCurr: ${pivotCurr}")
      var nodes = partitionOnEq(initData.head, initData)
      //println(s"nodes: ${nodes._1.toList}\n${nodes._2.toList}\n${nodes._3.toList}")

      while (err == 0 && !MedianIndexes.isEmpty) {
        err = nodes match {
          case ( _, pivoted, _) if (pivoted.isEmpty) => { println("ERROR: empty pivot column."); -1}
          case (lower, pivoted, upper) =>
            //println(s"nodes: \n${nodes._1.toList}\n${nodes._2.toList}\n${nodes._3.toList}")
            //BOTH INDEXES IN LOWER SEGMENT
            if (0 <= MedianIndexes.head && MedianIndexes.last < lower.length) {
              //println
              //println("BOTH INDEXES IN LOWER SEGMENT")
              pivotUB = pivotCurr
              //println(s"pivotUB: ${pivotUB}")
              lower.find(e => pivotLB < e && e < pivotUB) match {
                case None => { println("ERROR: bad pivot 0."); -2 }
                case Some(pivot:Int) => {
                  //println(s"pivot: ${pivot}")
                  pivotCurr = pivot
                  nodes = partitionOnEq(pivotCurr, lower)
                  0
                }
              }
            }
            //HIGHER INDEX IN PIVOTED SEGMENT
            else if (0 <= MedianIndexes.head && MedianIndexes.head < lower.length) {
              //println
              //println("HIGHER INDEX IN PIVOTED SEGMENT")
              if (1 < MedianIndexes.length) {
                MedianValues += pivoted.head
                MedianIndexes.remove(1)
              }
              pivotUB = pivotCurr
              //println(s"pivotUB: ${pivotUB}")
              lower.find(e => pivotLB < e && e < pivotUB) match {
                case None => { println("ERROR: bad pivot 1."); -2 }
                case Some(pivot:Int) => {
                  pivotCurr = pivot
                  nodes = partitionOnEq(pivotCurr, lower)
                  0
                }
              }
            }
            //BOTH INDICES IN PIVOTED SEGMENT
            else if (lower.length <= MedianIndexes.head && MedianIndexes.last < (lower.length +pivoted.length)) {
              //println
              //println("BOTH INDICES IN PIVOTED SEGMENT")
              MedianValues += pivoted.head
              MedianIndexes.remove(0)
              if (0 < MedianIndexes.length) {
                MedianValues += pivoted.head
                MedianIndexes.remove(0)
              }
              0
            }
            //LOWEST INDEX IN PIVOTED, HIGHER INDEX NOT
            else if (lower.length <= MedianIndexes.head && MedianIndexes.head < (lower.length +pivoted.length)) {
              //println
              //println("LOWEST INDEX IN PIVOTED, HIGHER INDEX NOT")
              MedianValues += pivoted.head
              MedianIndexes.remove(0)
              if (0 < MedianIndexes.length) {
                pivotLB = pivotCurr
                //println(s"pivotLB: ${pivotLB}")
                MedianIndexes = MedianIndexes.map(index => index -(lower.length +pivoted.length))
                upper.find(e => pivotLB < e && e < pivotUB) match {
                  case None => { println("ERROR: bad pivot 2."); -2 }
                  case Some(pivot:Int) => {
                    pivotCurr = pivot
                    nodes = partitionOnEq(pivotCurr, upper)
                  }
                }
              }
              0
            }
            //BOTH INDEXES IN UPPER SEGMENT
            else if ((lower.length +pivoted.length) <= MedianIndexes.head && MedianIndexes.last < (lower.length +pivoted.length +upper.length)) {
              //println
              //println("BOTH INDEXES IN UPPER SEGMENT")
              pivotLB = pivotCurr
              //println(s"pivotLB: ${pivotLB}")
              MedianIndexes = MedianIndexes.map(index => index -(lower.length +pivoted.length))
              upper.find(e => pivotLB < e && e < pivotUB) match {
                case None => { println("ERROR: bad pivot 3."); -2 }
                case Some(pivot:Int) => {
                  pivotCurr = pivot
                  nodes = partitionOnEq(pivotCurr, upper)
                }
              }
              0
            }
            else {



              println
              println("ERROR: bad case option."); -3 }
        }
        //println(s"err: ${err}")
      }
      println
      println("results: ")
      println(MedianValues.toList)
      //println("nodes:")
      //nodes.foreach(nd => println(nd.isSorted  +" : " +nd.data.toList))
      err
    }

    val rndData = collection.mutable.ArrayBuffer.fill(5000000)(scala.util.Random.nextInt(1000))
    median(rndData.toArray)
  }

  Median


  /*
  object MedianRecurse {
    import scala.collection.mutable._

    case class DataNode(isSorted:Boolean, data:Array[Int]) {
      def printNode = println(s"\nisSorted: ${isSorted}\ndata: ${data.toList}")
    }
    case class InfoNode(lb:Int, ub:Int, medianVals:List[Int], medianOffsets:List[Int]) {
      def printInfo() = println(s"\nmedianVals: ${medianVals.toList}\noffsets: ${medianOffsets.toList}")
    }


    def partitionOnEq(k: Int, data: Array[Int]): Array[DataNode] = {
      type abi = ArrayBuffer[Int]
      data match {
        case v if (v.isEmpty) => Array.empty[DataNode]
        case v if (!v.isEmpty) => {
          val res = data.foldLeft((new abi, new abi, new abi)) {
            case ((acl:abi, ace:abi, acg:abi), i) =>
              if (i == k)       (acl, ace :+ i, acg)
              else if (i < k)   (acl :+ i, ace, acg)
              else              (acl, ace, acg :+ i)
          }
          val ret = ArrayBuffer.empty[DataNode]
          ret.append(new DataNode(res._1.length <= 1, res._1.toArray))
          ret.append(new DataNode(true, res._2.toArray))
          ret.append(new DataNode(res._3.length <= 1, res._3.toArray))
          ret.toArray
        }
      }
    }

    def getNodeIndex(offset:Int, nodes:Array[DataNode]): (Int, Int) = {
      var offsetCpy = offset
      val datalen = nodes.length
      if (datalen <= 0) { println("getMedianNodeIndex ERROR: no data nodes."); (-1, -1) } else {
        var index = 0
        while (index < datalen && nodes(index).data.length <= offsetCpy) {
          offsetCpy -= nodes(index).data.length
          index += 1
        }
        (index, offsetCpy)
      }
    }

    def median(data: Array[Int]): Int = {

      def recurse(info:InfoNode, node:DataNode): Int  = {

        println
        println("recurse:")
        info.printInfo
        node.printNode

        //NO DATA
        if (node.data.length == 0){
          println("ERROR: data length 0.")
          -1
        }
        //SORTED
        else if (node.isSorted) {
          //println("sorted:")
          //medianOffsets.foreach(offset => println(offset.toString))

          if (0 < node.data.length) {
          }



          info.medianOffsets.foldLeft(0)((ac, index) => ac +node.data(index))/info.medianOffsets.length
        }
        //UNSORTED
        else {

          if (!info.medianOffsets.isEmpty) {
            val pivot = node.data.find(e => info.lb < e && e < info.ub) match {
              case Some(p: Int) => p
              case None => { println("ERROR: bad pivot."); -1 }
            }
            println(s"pivot: ${pivot}")

            val parts = partitionOnEq(pivot, node.data)
            val (part_index, med_offset) = getNodeIndex(info.medianOffsets.head, parts)
            val newMedianOffsets = info.medianOffsets.map(x => x -med_offset)
            val (lb:Int, ub:Int) = part_index match {
              case 0 => (info.lb, pivot)
              case 1 => (pivot, pivot)
              case 2 => (pivot, info.ub)
            }
            recurse(new InfoNode(lb, ub, info.medianVals, info.medianOffsets), parts(part_index))
          }
          else { println("ERROR: no median offsets"); -1 }
        }
      }
      val medianOffsets = data.length/2 :: (if (data.length % 2 == 0) data.length/2 -1 :: Nil else Nil)
      recurse(new InfoNode(Int.MinValue, Int.MaxValue, Nil, medianOffsets), new DataNode(false, data))
    }

    val data_size = 100
    val lower_bound = 0
    val upper_bound = 100
    val rndData = collection.mutable.ArrayBuffer.fill(data_size)(scala.util.Random.nextInt(upper_bound))

    median(rndData.toArray)


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
  */
  //MedianRecurse

}
