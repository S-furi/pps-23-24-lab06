package ex2

import org.junit.jupiter.api.{BeforeAll, Test}
import org.junit.jupiter.api.TestInstance
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.TestInstance.Lifecycle

import scala.collection.mutable

@TestInstance(Lifecycle.PER_CLASS)
class ConferenceReviewingTest:

  private val cr: ConferenceReviewing = ConferenceReviewing()

  @BeforeAll def init(): Unit =
    cr.loadReview(1, 8, 8, 6, 8)
    cr.loadReview(1, 9, 9, 6, 9)
    cr.loadReview(2, 9, 9, 10, 9)
    cr.loadReview(2, 4, 6, 10, 6)
    cr.loadReview(3, 3, 3, 3, 3)
    cr.loadReview(3, 4, 4, 4, 4)
    cr.loadReview(4, 6, 6, 6, 6)
    cr.loadReview(4, 7, 7, 8, 7)

    val map: Map[Question, Int] = Map(
      Question.Relevance -> 8,
      Question.Significance -> 8,
      Question.Confidence -> 7,
      Question.Final -> 8
    )

    cr.loadReview(4, map)
    cr.loadReview(5, 6, 6, 6, 10) // 6.0
    cr.loadReview(5, 7, 7, 7, 10) // 7.0

  @Test def testOrderedScores(): Unit =
    assertEquals(cr.orderedScores(2, Question.Relevance), List(4, 9))
    assertEquals(cr.orderedScores(4, Question.Confidence), List(6, 7, 8))
    assertEquals(cr.orderedScores(5, Question.Final), List(10, 10))

  @Test def testAverageFinalScore(): Unit =
    assertEquals(cr.averageFinalScore(1), 8.5, 0.01)
    assertEquals(cr.averageFinalScore(2), 7.5, 0.01)
    assertEquals(cr.averageFinalScore(3), 3.5, 0.01)
    assertEquals(cr.averageFinalScore(4), 7.0, 0.01)
    assertEquals(cr.averageFinalScore(5), 10.0, 0.01)

  @Test def testAcceptedArticles(): Unit =
    assertEquals(cr.acceptedArticles(), Set(1, 2, 4))

  @Test def testSortedAcceptedArticles(): Unit =
    assertEquals(cr.sortedAcceptedArticles(), List( (4, 7.0), (2, 7.5), (1, 8.5)))

  @Test def optionalTestAverageWeightedFinalScore(): Unit =
    assertEquals(cr.averageWeightedFinalScoreMap()(1), (4.8 + 5.4) / 2.0, 0.001)
    assertEquals(cr.averageWeightedFinalScoreMap()(2), (9.0 + 6.0) / 2, 0.01);
    assertEquals(cr.averageWeightedFinalScoreMap()(3), (0.9 + 1.6) / 2, 0.01);
    assertEquals(cr.averageWeightedFinalScoreMap()(4), (3.6 + 5.6 + 5.6) / 3, 0.01);
    assertEquals(cr.averageWeightedFinalScoreMap()(5), (6.0 + 7.0) / 2, 0.01);
    assertEquals(cr.averageWeightedFinalScoreMap().keys.size, 5);