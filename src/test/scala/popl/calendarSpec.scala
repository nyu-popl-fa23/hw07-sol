package popl

import calendar._
import org.scalatest.flatspec.AnyFlatSpec

class calendarSpec extends AnyFlatSpec:
  // Part 3: Using Lists to Print Calendars

  // add your unit tests for Part 3 here ...

  "unlines" should "add a newline character between each line" in {
    assert(unlines(List.empty) === List.empty)
    assert(unlines(List(List.empty)) === List.empty)
    assert(unlines(List(List('f','e','i','s','t','y'),List('f','a','w','n'))) === List('f','e','i','s','t','y','\n','f','a','w','n'))
  }

  "firstDay" should "return the weekday of the first day in the given month" in {
    assert(firstDay(1, 2018) === 1)
    assert(firstDay(2, 2018) === 4)
    assert(firstDay(12, 2015) === 2)
  }

  "firstDay" should "throw an exception when using an invalid month" in {
    intercept[java.lang.IllegalArgumentException] {
      firstDay(0, 2018)
    }

    intercept[java.lang.IllegalArgumentException] {
      firstDay(13, 2018)
    }
  }

  "above" should "place this Picture above the other Picture" in {
    assert ((pixel('a') above pixel('b')).toString === "a\nb")
  }

  "above" should "throw an exception when applied to pictures of incompatible width" in {
    intercept[java.lang.IllegalArgumentException] {
      pixel('a') above (pixel('b') beside pixel('c'))
    }
  }

  "beside" should "place this Picture beside the other Picture" in {
    assert ((pixel('a') beside pixel('b')).toString === "ab")
  }

  "beside" should "throw an exception when applied to pictures of incompatible height" in {
    intercept[java.lang.IllegalArgumentException] {
      pixel('a') beside (pixel('b') above pixel('c'))
    }
  }

  "stack" should "place all pictures in the list above one another" in {
    assert (stack(List(pixel('a'), pixel('b'), pixel('c'))).toString === "a\nb\nc")
  }

  "spread" should "place all pictures in the list beside eachother" in {
    assert (spread(List(pixel('a'), pixel('b'), pixel('c'))).toString === "abc")
  }

  "tile" should "tile the list of list of pictures" in {
    assert (tile(List(List(pixel('a'), pixel('b')), List(pixel('c'), pixel('d')))).toString === "ab\ncd")
  }

  "rightJustify" should "right justify the list of characters to the given width" in {
    assert(rightJustify(2)(List('a', 'b')).toString === List('a', 'b').mkString)
    assert(rightJustify(3)(List('a', 'b')).toString === List(' ', 'a', 'b').mkString)
    assert(rightJustify(4)(List('a', 'b')).toString === List(' ', ' ', 'a', 'b').mkString)
  }

  val cal_2015_02 =
    """ Su Mo Tu We Th Fr Sa
      |  1  2  3  4  5  6  7
      |  8  9 10 11 12 13 14
      | 15 16 17 18 19 20 21
      | 22 23 24 25 26 27 28
      |                     
      |                     """.stripMargin


  val cal_2023_09 =
    """ Su Mo Tu We Th Fr Sa
      |                 1  2
      |  3  4  5  6  7  8  9
      | 10 11 12 13 14 15 16
      | 17 18 19 20 21 22 23
      | 24 25 26 27 28 29 30
      |                     """.stripMargin

  val cal_2023_05 =
    """ Su Mo Tu We Th Fr Sa
      |     1  2  3  4  5  6
      |  7  8  9 10 11 12 13
      | 14 15 16 17 18 19 20
      | 21 22 23 24 25 26 27
      | 28 29 30 31         
      |                     """.stripMargin
  
  "calendar" should "print the calendar of the given month" in {
    assert(calendar(2015, 2).toString === cal_2015_02)
    assert(calendar(2023, 9).toString === cal_2023_09)
    //assert(calendar(2023, 5).toString === cal_2023_05)
  }