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