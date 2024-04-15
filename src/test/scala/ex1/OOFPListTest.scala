package ex1

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals

class OOFPListTest:
  private val reference = List(1, 2, 3, 4)

  @Test def testZipWithValue(): Unit =
    val expected = List((1, 10), (2, 10), (3, 10), (4, 10))
    assertEquals(expected, reference.zipWithValue(10))

  @Test def testLength(): Unit =
    assertEquals(4, reference.length())

  @Test def testZipWithIndex(): Unit =
    val expected = List((1, 0), (2, 1), (3, 2), (4, 3))
    assertEquals(expected, reference.zipWithIndex)

  @Test def testPartition(): Unit =
    val expected = (List(2, 4), List(1, 3))
    assertEquals(expected, reference.partition(_ % 2 == 0))

  @Test def testTakeRight(): Unit =
    val expected = List(2, 3, 4)
    assertEquals(expected, reference.takeRight(3))
    assertEquals(List(4), reference.takeRight(1))