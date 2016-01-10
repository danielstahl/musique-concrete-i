package music

import net.soundmining.{MusicPlayer, BusGenerator}
import net.soundmining.Instrument._
import net.soundmining.Utils._

/**
  * Musique Concrete I
  */
object MusiqueConcrete1 {

  def cut1(start: Float, ampBus: Int)(implicit player: MusicPlayer) = {
    val playFasterCut = (0 until 4).flatMap(i =>
      new PlayStereoSoundInstrumentBuilder()
        .ampBus.bus(ampBus)
        .bufNum(0)
        .scale(1.1f + (i * 0.1f))
        .buildInstruments()).toSeq

    val playFastCut = (0 until 4).flatMap(i =>
      new PlayStereoInverseSoundInstrumentBuilder()
        .ampBus.bus(ampBus)
        .bufNum(0)
        .scale(0.4f + (i * 0.1f))
        .buildInstruments()).toSeq


    val playSlowCut = (0 until 4).flatMap(i =>
      new PlayStereoInverseSoundInstrumentBuilder()
        .ampBus.bus(ampBus)
        .bufNum(0)
        .scale(0.1f + (i * 0.003f))
        .buildInstruments()).toSeq

    val playFasterCut2 = (0 until 6).flatMap(i =>
      new PlayStereoSoundInstrumentBuilder()
        .ampBus.bus(ampBus)
        .bufNum(0)
        .scale(1.05f + (i * 0.45f))
        .buildInstruments()).toSeq

    player.sendNew(absoluteTimeToMillis(start), playFasterCut ++ playFastCut ++ playSlowCut)
    player.sendNew(absoluteTimeToMillis(start + 1.9f), playFasterCut2)
  }

  def klang1(start: Float, ampBus: Int)(implicit player: MusicPlayer) = {
    val klangLong1 =
      new PlayStereoInverseSoundInstrumentBuilder()
      .ampBus.bus(ampBus)
      .bufNum(1)
      .scale(0.1f)
      .buildInstruments()

    val klangLong2 =
      new PlayStereoSoundInstrumentBuilder()
        .ampBus.bus(ampBus)
        .bufNum(1)
        .scale(0.5f)
        .buildInstruments()

    val klangLong3 =
      new PlayStereoSoundInstrumentBuilder()
        .ampBus.bus(ampBus)
        .bufNum(1)
        .scale(0.05f)
        .buildInstruments()

    val highKlang1 = (0 until 6).flatMap(i =>
      new PlayStereoInverseSoundInstrumentBuilder()
        .ampBus.bus(ampBus)
        .bufNum(1)
        .scale(1.5f + (i * 0.7f))
        .buildInstruments()).toSeq

    player.sendNew(absoluteTimeToMillis(start), klangLong1)
    player.sendNew(absoluteTimeToMillis(start + 1.6f), klangLong2)
    player.sendNew(absoluteTimeToMillis(start + 2.0f), klangLong3)
    player.sendNew(absoluteTimeToMillis(start + 2.4f), highKlang1)

  }

  def main(args: Array[String]): Unit = {
    BusGenerator.reset()
    implicit val player: MusicPlayer = MusicPlayer()

    player.startPlay()

    setupNodes(player)

    player.sendBundle(0, Seq(player.makeAllocRead(0, "/Users/danielstahl/Documents/Projects/musique-concrete-i_sounds/cut_1.aiff")))
    player.sendBundle(0, Seq(player.makeAllocRead(1, "/Users/danielstahl/Documents/Projects/musique-concrete-i_sounds/short_klang_1.aiff")))

    val ampBus = LineControlInstrumentBuilder
      .line(10, 1, 1)
      .out(BusGenerator.nextControl())
    player.sendNew(0, ampBus.buildInstruments())

    cut1(0f, ampBus.out)
    klang1(2.0f, ampBus.out)

    Thread.sleep(5000)
  }
}
