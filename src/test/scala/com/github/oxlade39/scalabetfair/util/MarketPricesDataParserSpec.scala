package com.github.oxlade39.scalabetfair.util

import org.specs2.mutable.Specification

/**
 * Created with IntelliJ IDEA.
 * User: pic
 * Date: 9/27/13
 * Time: 12:18 AM
 */
class MarketPricesDataParserSpec extends Specification {

  import MarketPricesDataParser._

  "inflatedMarketPrices" should {

    "inflate market prices from compressed string representation" in {

      val imp = inflateMarketPrices(
        "111179500~GBP~ACTIVE~0~1~~true~5.0~1380229928071~~Y:6307039~1~4.0~3.25~~~false~6.0~6.0~~|6.0~5.74~L~1~4.6~2.17~L~2~4.0~6.84~L~3~|14.0~3.36~B~1~14.5~8.0~B~2~15.0~5.0~B~3~:7584587~2~0.0~~~~false~Infinity~15.5~~|5.0~5.0~L~1~2.36~2.0~L~2~2.34~2.0~L~3~|15.5~3.36~B~1~16.0~8.8~B~2~17.5~4.0~B~3~:6559569~3~0.0~~~~false~Infinity~13.0~~|5.0~5.0~L~1~4.2~2.93~L~2~3.85~19.3~L~3~|16.0~3.28~B~1~16.5~8.8~B~2~17.5~4.0~B~3~:7705225~4~638.9~2.3~~~false~1.18~2.13~~|2.16~10.0~L~1~2.14~30.0~L~2~2.08~30.0~L~3~|2.56~16.81~B~1~2.6~6.68~B~2~2.66~7.61~B~3~:6322123~5~0.0~~~~false~Infinity~15.5~~|5.1~3.05~L~1~5.0~5.0~L~2~3.2~25.0~L~3~|18.5~3.36~B~1~19.0~4.0~B~2~19.5~8.8~B~3~:7374769~6~0.0~~~~false~Infinity~13.07~~|4.0~5.0~L~1~3.35~2.0~L~2~3.3~2.0~L~3~|14.0~3.36~B~1~14.5~8.0~B~2~15.0~5.0~B~3~"
      )

      // 111179500~GBP~ACTIVE~0~1~~true~5.0~1380229928071~~Y: // market data
      // 6307039~1~4.0~3.25~~~false~6.0~6.0~~|6.0~5.74~L~1~4.6~2.17~L~2~4.0~6.84~L~3~|14.0~3.36~B~1~14.5~8.0~B~2~15.0~5.0~B~3~: // ri 0
      // 7584587~2~0.0~~~~false~Infinity~15.5~~|5.0~5.0~L~1~2.36~2.0~L~2~2.34~2.0~L~3~|15.5~3.36~B~1~16.0~8.8~B~2~17.5~4.0~B~3~: // ri 1
      // 6559569~3~0.0~~~~false~Infinity~13.0~~|5.0~5.0~L~1~4.2~2.93~L~2~3.85~19.3~L~3~|16.0~3.28~B~1~16.5~8.8~B~2~17.5~4.0~B~3~: // ri 2
      // 7705225~4~638.9~2.3~~~false~1.18~2.13~~|2.16~10.0~L~1~2.14~30.0~L~2~2.08~30.0~L~3~|2.56~16.81~B~1~2.6~6.68~B~2~2.66~7.61~B~3~: // ri 3
      // 6322123~5~0.0~~~~false~Infinity~15.5~~|5.1~3.05~L~1~5.0~5.0~L~2~3.2~25.0~L~3~|18.5~3.36~B~1~19.0~4.0~B~2~19.5~8.8~B~3~: // ri 4
      // 7374769~6~0.0~~~~false~Infinity~13.07~~|4.0~5.0~L~1~3.35~2.0~L~2~3.3~2.0~L~3~|14.0~3.36~B~1~14.5~8.0~B~2~15.0~5.0~B~3~  // ri 5

      imp.runnersInfo should have size(6)

      imp.runnersInfo.foreach { ri: RunnerInfo =>
        ri.backPrices should have size(3)
        ri.layPrices should have size(3)
      }

      val ri2 = imp.runnersInfo(2)

      // 6559569~3~0.0~~~~false~Infinity~13.0~~| // selection data
      // 5.0~5.0~L~1~4.2~2.93~L~2~3.85~19.3~L~3~| // back prices
      // 16.0~3.28~B~1~16.5~8.8~B~2~17.5~4.0~B~3~: // lay prices
      ri2.selectionId shouldEqual 6559569

      val bps = ri2.backPrices

      // 5.0~5.0~L~1~ // bps 0
      // 4.2~2.93~L~2~ // bps 1
      // 3.85~19.3~L~3~| // bps 2
      bps(1).price shouldEqual BigDecimal("4.2")
      bps(1).amount shouldEqual BigDecimal("2.93")
      bps(2).price shouldEqual BigDecimal("3.85")

      success
    }

    "inflate market prices from compressed string representation (betfair doc example, adjusted)" in {

      val imp = inflateMarketPrices(
        "5082333~GBP~ACTIVE~0~1~NR\\: (RSA) <br>8. Fan Mail (0%,11\\:07), 6(2.5%,11\\:08)~true~5.0~1162835723938~6. Earlswood,9.08,2.5;8. Fan Mail,9.07,2.4~Y:1058616~0~6.04~8.4~~11.9~false||:670160~1~6.18~17.5~~4.2~false||:1132008~2~9.78~5.2~~20.4~false||:894820~3~140.02~4.6~~20.4~false||1.01~5.0~B~1~:1414415~4~8.2~6.2~~16.0~false||:575636~5~5.54~11.5~~8.6~false||:1433455~6~0.0~~~0.4~false||:1433456~7~0.0~~~0.9~false||:746753~8~5.54~11.5~~5.2~false||:1433457~9~0.0~~~4.2~false||:1147548~10~0.0~~~2.6~false||:1433458~11~62.46~2.0~~3.5~false||:1433459~12~0.0~~~ 0.9~false||:1433460~13~0.0~~~0.9~false||"
      )

      imp.runnersInfo should have size(14)

      imp.runnersInfo.foreach { ri: RunnerInfo =>
        ri.backPrices.size should be_<=(3)
        ri.layPrices.size should be_<=(3)
      }

      val ri3 = imp.runnersInfo(3)

      ri3.selectionId shouldEqual 894820

      val lps = ri3.layPrices

      lps should have size(1)

      lps(0).price shouldEqual BigDecimal("1.01")

      success
    }

  }
}
