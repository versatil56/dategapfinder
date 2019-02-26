package example

import org.joda.time.DateTime
import org.scalatest._

class DateGapFinderspec extends FlatSpec with Matchers {
  "A date gap finder" should "find gaps with one second increments" in {
    val date = new DateTime(2018, 1, 1, 22, 0)
    val date2 = new DateTime(2018, 1, 1, 22, 2)


    val dates = Seq(date, date2)
    val finder = DateGapFinder(dates)

    val result: Seq[DateTime] = finder.findWithIncrement(1)

    result.head shouldBe new DateTime(2018, 1, 1, 22, 1)
  }

  it should "deal with bigger gaps than the increment" in {
    val date = new DateTime(2018, 1, 1, 22, 0)
    val date2 = new DateTime(2018, 1, 1, 22, 3)


    val dates = Seq(date, date2)
    val finder = DateGapFinder(dates)

    val result: Seq[DateTime] = finder.findWithIncrement(1)

    result.head shouldBe new DateTime(2018, 1, 1, 22, 1)
    result.last shouldBe new DateTime(2018, 1, 1, 22, 2)
  }

  it should "deal with uneven gaps with the same increment" in {
    val date = new DateTime(2018, 1, 1, 22, 0)
    val date2 = new DateTime(2018, 1, 1, 22, 3)
    val date3 = new DateTime(2018, 1, 1, 22, 5)


    val dates = Seq(date, date2, date3)
    val finder = DateGapFinder(dates)

    val result: Seq[DateTime] = finder.findWithIncrement(1)
    val expectedResult = Seq(new DateTime(2018, 1, 1, 22, 1), new DateTime(2018, 1, 1, 22, 2), new DateTime(2018, 1, 1, 22, 4))

    result shouldBe expectedResult
  }

  it should "deal with gaps when there is consecutive data that is already there" in {
    val date = new DateTime(2018, 1, 1, 22, 0)
    val date2 = new DateTime(2018, 1, 1, 22, 1)
    val date3 = new DateTime(2018, 1, 1, 22, 5)


    val dates = Seq(date, date2, date3)
    val finder = DateGapFinder(dates)

    val result: Seq[DateTime] = finder.findWithIncrement(1)
    val expectedResult = Seq(new DateTime(2018, 1, 1, 22, 2), new DateTime(2018, 1, 1, 22, 3), new DateTime(2018, 1, 1, 22, 4))

    result shouldBe expectedResult
  }

  it should "handle a whole minute with data in the middle" in {
    val date = new DateTime(2018, 1, 1, 22, 0)
    val date2 = new DateTime(2018, 1, 1, 22, 15)
    val date3 = new DateTime(2018, 1, 1, 22, 59)


    val dates = Seq(date, date2, date3)
    val finder = DateGapFinder(dates)

    val result: Seq[DateTime] = finder.findWithIncrement(1)

    result.size shouldBe 57
  }


  case class DateGapFinder(dates: Seq[DateTime]) {
    def findWithIncrement(minutes: Int): Seq[DateTime] = {
      dates.foldLeft(Seq[DateTime]())((missingDates, currentDate) => {
        val expectedNextDate: DateTime = currentDate.plusMinutes(minutes)

        if (isMissingDate(expectedNextDate) && isNotLastDate(currentDate)) {
          val nextAvailableDate = dates(dates.indexOf(currentDate) + 1)

          FillGaps(missingDates :+ expectedNextDate, expectedNextDate, nextAvailableDate)
        }
        else {
          missingDates
        }
      })
    }

    private def FillGaps(missingDates: Seq[DateTime], expectedNextDate: DateTime, nextAvailableDate: DateTime): Seq[DateTime] = {
      val nextMissingDate = missingDates.last.plusMinutes(1)

      if (nextMissingDate == nextAvailableDate) {
        missingDates
      } else {
        FillGaps(missingDates :+ nextMissingDate, nextMissingDate, nextAvailableDate)
      }
    }

    private def isMissingDate(expectedNextDate: DateTime) = {
      !dates.contains(expectedNextDate)
    }

    private def isNotLastDate(currentDate: DateTime) = {
      !(currentDate == dates.last)
    }
  }

}
