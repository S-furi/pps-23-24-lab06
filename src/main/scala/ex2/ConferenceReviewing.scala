package ex2

import scala.collection.immutable

enum Question:
  case Relevance
  case Significance
  case Confidence
  case Final

trait ConferenceReviewing:

  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[(Int, Double)]
  def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = new ConferenceReviewingImpl()
  private def minimumAcceptanceFinalScore: Int = 5
  private def minimumAcceptanceRelevanceScore: Int = 8

  private class ConferenceReviewingImpl() extends ConferenceReviewing:
    private var articlesScores = Map[Int, List[Map[Question, Int]]]()

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      val updatedScores: List[Map[Question, Int]] = articlesScores.getOrElse(article, List()) ++ List(scores)
      articlesScores = articlesScores.filterNot(_._1 == article) ++ Map(article -> updatedScores)

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      loadReview(article, Map(
        Question.Relevance -> relevance,
        Question.Significance -> significance,
        Question.Confidence -> confidence,
        Question.Final -> fin,
      ))

    override def orderedScores(article: Int, question: Question): List[Int] =
      articlesScores(article).map(m => m(question)).sortWith(_ < _)

    override def averageFinalScore(article: Int): Double =
      articlesScores(article).averageOn(Question.Final)

    override def acceptedArticles(): Set[Int] =
      articlesScores
        .filter(a => averageFinalScore(a._1) > minimumAcceptanceFinalScore)
        .filter(a => a._2.exists(_(Question.Relevance) >= minimumAcceptanceRelevanceScore))
        .keySet

    override def sortedAcceptedArticles(): List[(Int, Double)] =
      acceptedArticles()
        .map(a => a -> averageFinalScore(a))
        .toList
        .sortWith(_._2 < _._2)

    override def averageWeightedFinalScoreMap(): Map[Int, Double] =
      articlesScores
        .map(a => a._1 -> a._2.weightedAverage((acc, curr) =>
          acc + (curr(Question.Confidence) * curr(Question.Final) / 10)
        ))

    extension (scores: List[Map[Question, Int]])
      private def weightedAverage(avgFunc: (Double, Map[Question, Int]) => Double): Double =
        scores.foldLeft(0.0)(avgFunc) / scores.size.toDouble

      private def averageOn(q: Question): Double =
        weightedAverage(_ + _(q))

@main def main(): Unit =
  val cr = ConferenceReviewing()

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

  println(cr.averageWeightedFinalScoreMap()(1))
  println(cr.averageWeightedFinalScoreMap()(2))
  println(cr.averageWeightedFinalScoreMap()(3))
  println(cr.averageWeightedFinalScoreMap()(4))
  println(cr.averageWeightedFinalScoreMap()(5))
