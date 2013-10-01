package com.github.oxlade39.scalabetfair.util

/**
 * Created with IntelliJ IDEA.
 * User: pic
 * Date: 9/26/13
 * Time: 11:27 PM
 */

object MarketPricesDataParser {

  import ParseToolkit.escapedSplit

  class InvalidMarketPricesData extends RuntimeException
  object InvalidMarketPricesData extends InvalidMarketPricesData

  case class PriceInfo(val price: BigDecimal, val amount: BigDecimal, val priceType: String, val index: Int) {
    require { price > 1.0 && price <= 1000.0}
    require { amount >= 0.0 }
    require { index > 0}
    require { priceType == "L" || priceType == "B"}

    def backAvailable = if (priceType == "L") amount else BigDecimal(0)
    def layAvailable = if (priceType == "B") amount else BigDecimal(0)
  }

  case class RunnerInfo(
    val selectionId: Int,
    val orderIndex:Int,
    val totalAmountMatched: BigDecimal,
    val lastPriceMatched: Option[BigDecimal],
    val reductionFactor: Option[BigDecimal],
    val handicap: Option[BigDecimal],
    val vacant: Boolean,
    val farSPPrice: Option[BigDecimal],
    val nearSPPrice: Option[BigDecimal],
    val actualSPPrice: Option[BigDecimal],
    val backPrices: List[PriceInfo],
    val layPrices: List[PriceInfo]
  )

  case class InflatedMarketPrices(val marketData: MarketData, val runnersInfo: List[RunnerInfo])

  def  inflateMarketPrices(data: String): InflatedMarketPrices = {

    val parts = escapedSplit(data, ":")

    if (parts.isEmpty)
      throw InvalidMarketPricesData
    else {
      val baseData = extractMarketData(parts(0))
      val runnersInfo = parts.slice(1, parts.length).map { data: String => extractRunnerData(data) }.toList
      InflatedMarketPrices(baseData, runnersInfo)
    }
  }

  case class MarketData(
    val marketId: Int,
    val currencyCode: String,
    val marketStatus: String,
    val delay: Int,
    val numberOfWinners: Int,
    val marketInfo: String,
    val discountAllowed: Boolean,
    val marketBaseRate: BigDecimal,
    val lastRefresh: Long,
    val removedRunners: String,
    val bspMarket: Boolean
  )

  private def extractMarketData(data: String) = {

    val parts = escapedSplit(data, "~", -1)

    if (parts.length != 11)
      throw InvalidMarketPricesData
    else
      MarketData(
        marketId = parts(0).toInt,
        currencyCode = parts(1),
        marketStatus = parts(2),
        delay = parts(3).toInt,
        numberOfWinners = parts(4).toInt,
        marketInfo = parts(5),
        discountAllowed = parts(6).toBoolean,
        marketBaseRate = parts(7).toDouble,
        lastRefresh = parts(8).toLong,
        removedRunners = parts(9),
        bspMarket = "Y" == parts(10)
      )

  }

  private def extractRunnerData(data: String): RunnerInfo = {

    val parts = escapedSplit(data, """\|""", -1)

    if (parts.isEmpty || parts.length > 3)
      throw InvalidMarketPricesData
    else {

      // first part has general info about the selection, second part has back prices, third part has lay inflateMarketPrices
      val selectionParts = escapedSplit(parts.head, "~", -1).toArray

      if (selectionParts.length < 7) // plus 3 fields about SP that may might be missing
        throw InvalidMarketPricesData

      val backPrices: List[PriceInfo] = if (!parts.tail.isEmpty) {
        priceInfos(escapedSplit(parts.tail.head, "~"))
      }
      else
        Nil

      val layPrices: List[PriceInfo] = if (!parts.tail.isEmpty && !parts.tail.tail.isEmpty)
        priceInfos(escapedSplit(parts.tail.tail.head, "~"))
      else
        Nil

      val (farSPPrice, nearSPPrice, actualSPPrice) = if (selectionParts.length >= 10)
        (
          toBigDecimal(selectionParts(7)),
          toBigDecimal(selectionParts(8)),
          toBigDecimal(selectionParts(9))
        )
      else
        (
          None,
          None,
          None
        )

      RunnerInfo(
        selectionId = selectionParts(0).toInt,
        orderIndex = selectionParts(1).toInt,
        totalAmountMatched = BigDecimal(selectionParts(2)),
        lastPriceMatched = toBigDecimal(selectionParts(3)),
        reductionFactor = toBigDecimal(selectionParts(4)),
        handicap = toBigDecimal(selectionParts(5)),
        vacant = selectionParts(6).toBoolean,
        farSPPrice = farSPPrice,
        nearSPPrice = nearSPPrice,
        actualSPPrice = actualSPPrice,
        backPrices = backPrices,
        layPrices = layPrices
      )
    }
  }

  private def priceInfos(parts: List[String]): List[PriceInfo] = {
    val (f, s) = parts.splitAt(4)
    if (f.length < 2)
      Nil
    else {
      val pi = PriceInfo(
        BigDecimal(f(0)),
        BigDecimal(f(1)),
        f(2),
        f(3).toInt
      )
      if (s.isEmpty)
        List(pi)
      else
        pi :: priceInfos(s)
    }
  }

  private def toBigDecimal(s: String): Option[BigDecimal] = {
    try {
      Some(BigDecimal(s))
    } catch {
      case e: NumberFormatException => {
        None
      }
    }
  }


}
