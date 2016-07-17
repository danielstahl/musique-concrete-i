package music

import net.soundmining.{PatternItem, MusicPlayer, BusGenerator}
import net.soundmining.Instrument._
import net.soundmining.Utils._
import net.soundmining.Pattern._
import net.soundmining.Melody._

/**
  * Musique Concrete I
  */
object MusiqueConcrete1 {

  def cut(start: Float)(implicit player: MusicPlayer) = {
    val overallRepeats = 29

    val amps = palindrome(atom(.1f), atom(.2f), atom(.1f), atom(3f), atom(.3f), atom(2.1f), atom(.8f))

    val repets = cycle(atom(7), atom(3), cycle(atom(3), atom(18)), atom(11), cycle(atom(7), atom(3)), atom(4), atom(11))

    val times = cycle(atom(3f), atom(3f), atom(1f), atom(1f), atom(3f), atom(7f), atom(3f), atom(4f), atom(1f), atom(1f))

    val startScale = palindrome(atom(.1f), atom(.4f), atom(.3f), atom(.3f), atom(.1f), atom(.4f))

    val scale = palindrome(atom(.02f), atom(.1f), atom(.02f), atom(.1f), atom(.03f), atom(.03f), atom(.02f))

    val stereoFn = () => new PlayStereoSoundInstrumentBuilder
    val inverseStereoFn = () => new PlayStereoInverseSoundInstrumentBuilder

    val instrumentBuilder: PatternItem[() => CommonPlayStereoSoundInstrumentBuilder] =
      cycle(atom(stereoFn), atom(stereoFn), atom(inverseStereoFn), atom(stereoFn), atom(inverseStereoFn), atom(inverseStereoFn))

    val fasterCuts = (0 until overallRepeats).map(i =>
      (0 until repets.takeItem()).flatMap(j =>
        instrumentBuilder.takeItem().apply()
          .bufNum(0)
          .amp(amps.takeItem())
          .scale(startScale.takeItem() + (j * scale.takeItem()))
          .buildInstruments()).toSeq
      )

    val absoluteTimes = absolute(0f, times.stream.take(overallRepeats))

    (0 until overallRepeats).foreach(i =>
      player.sendNew(absoluteTimeToMillis(start + absoluteTimes(i)), fasterCuts(i))
    )
  }

  def klang(start: Float)(implicit player: MusicPlayer) = {
    val overallRepeats = 22

    val amps = palindrome(atom(.8f), atom(1.3f), atom(.5f), atom(3f), atom(.2f), atom(.3f), atom(.2f))

    val repets = cycle(atom(1), atom(2), cycle(atom(1), atom(3)), atom(2), cycle(atom(1), atom(3)), atom(1), atom(3))

    val times = cycle(atom(5f), atom(3f), atom(2f), atom(8f), atom(3f), atom(5f), atom(2f), atom(3f), atom(5f), atom(2f))

    val startScale = palindrome(atom(.1f), atom(.2f), atom(.3f), atom(.2f), atom(.1f), atom(.3f))

    val scale = palindrome(atom(.1f), atom(.02f), atom(.05f), atom(.1f), atom(.08f), atom(.05f), atom(.1f))

    val stereoFn = () => new PlayStereoSoundInstrumentBuilder
    val inverseStereoFn = () => new PlayStereoInverseSoundInstrumentBuilder

    val instrumentBuilder: PatternItem[() => CommonPlayStereoSoundInstrumentBuilder] =
      cycle(atom(stereoFn), atom(stereoFn), atom(inverseStereoFn), atom(stereoFn), atom(inverseStereoFn), atom(inverseStereoFn))

    val fasterKlang = (0 until overallRepeats).map(i =>
      (0 until repets.takeItem()).flatMap(j =>
        instrumentBuilder.takeItem().apply()
          .bufNum(1)
          .amp(amps.takeItem())
          .scale(startScale.takeItem() + (j * scale.takeItem()))
          .buildInstruments()).toSeq
    )

    val absoluteTimes = absolute(0f, times.stream.take(overallRepeats))

    (0 until overallRepeats).foreach(i =>
      player.sendNew(absoluteTimeToMillis(start + absoluteTimes(i)), fasterKlang(i))
    )
  }


  def lowCutNoise(start: Float)(implicit player: MusicPlayer) = {
    val overallRepeats = 10

    val amps = palindrome(atom(20f), atom(25f), atom(20f))

    val repets = cycle(atom(3), atom(2), cycle(atom(3), atom(4)))

    val times = cycle(atom(4f), atom(7f), atom(11f), atom(4f), atom(3f))

    val startScale = palindrome(atom(.01f), atom(.02f), atom(.03f))

    val scale = palindrome(atom(.002f), atom(.001f), atom(.002f))

    val soundStart = cycle(atom(0.7f), atom(0.7f), atom(0.5f), atom(0.7f), atom(0.5f))
    val soundStartVariance = cycle(atom(-.02f), atom(.01f), atom(.02f), atom(-.01f), atom(.01f))

    val stereoFn = () => new PlayStereoSoundInstrumentBuilder
    val inverseStereoFn = () => new PlayStereoInverseSoundInstrumentBuilder

    val instrumentBuilder: PatternItem[() => CommonPlayStereoSoundInstrumentBuilder] =
      cycle(atom(stereoFn), atom(stereoFn), atom(inverseStereoFn), atom(stereoFn), atom(inverseStereoFn), atom(inverseStereoFn))

    val lowNoises = (0 until overallRepeats).map(i =>
      (0 until repets.takeItem()).flatMap(j =>
        instrumentBuilder.takeItem().apply()
          .bufNum(0)
          .amp(amps.takeItem())
          .scale(startScale.takeItem() + (j * scale.takeItem()))
          .start(soundStart.takeItem() + soundStartVariance.takeItem())
          .buildInstruments()).toSeq
    )



    val absoluteTimes = absolute(0f, times.stream.take(overallRepeats))

    (0 until overallRepeats).foreach(i =>
      player.sendNew(absoluteTimeToMillis(start + absoluteTimes(i)), lowNoises(i))
    )
  }

  def lowKlangNoise(start: Float)(implicit player: MusicPlayer) = {
    val overallRepeats = 5

    val amps = palindrome(atom(20f), atom(25f), atom(20f))

    val repets = cycle(atom(1), atom(2), cycle(atom(1), atom(2)))

    val times = cycle(atom(11f), atom(18f), atom(11f), atom(11f), atom(18f))

    val startScale = palindrome(atom(.01f), atom(.02f), atom(.03f))

    val scale = palindrome(atom(.002f), atom(.001f), atom(.002f))

    val soundStart = cycle(atom(0.5f), atom(0.6f), atom(0.5f), atom(0.5f), atom(0.6f))
    val soundStartVariance = cycle(atom(-.02f), atom(.01f), atom(.02f), atom(-.01f), atom(.01f))

    val stereoFn = () => new PlayStereoSoundInstrumentBuilder
    val inverseStereoFn = () => new PlayStereoInverseSoundInstrumentBuilder

    val instrumentBuilder: PatternItem[() => CommonPlayStereoSoundInstrumentBuilder] =
      cycle(atom(stereoFn), atom(stereoFn), atom(inverseStereoFn), atom(stereoFn), atom(inverseStereoFn), atom(inverseStereoFn))

    val lowNoises = (0 until overallRepeats).map(i =>
      (0 until repets.takeItem()).flatMap(j =>
        instrumentBuilder.takeItem().apply()
          .bufNum(1)
          .amp(amps.takeItem())
          .scale(startScale.takeItem() + (j * scale.takeItem()))
          .start(soundStart.takeItem() + soundStartVariance.takeItem())
          .buildInstruments()).toSeq
    )

    val absoluteTimes = absolute(0f, times.stream.take(overallRepeats))

    (0 until overallRepeats).foreach(i =>
      player.sendNew(absoluteTimeToMillis(start + absoluteTimes(i)), lowNoises(i))
    )
  }


  def part1()(implicit player: MusicPlayer): Unit = {
    cut(0f)
    klang(0f)
  }

  def part2()(implicit player: MusicPlayer): Unit = {
    lowCutNoise(0f)
    lowKlangNoise(0f)
  }

  def main(args: Array[String]): Unit = {
    BusGenerator.reset()
    implicit val player: MusicPlayer = MusicPlayer()

    player.startPlay()

    setupNodes(player)

    player.sendBundle(0, Seq(player.makeAllocRead(0, "/Users/danielstahl/Documents/Projects/musique-concrete-i_sounds/cut_1.aiff")))
    player.sendBundle(0, Seq(player.makeAllocRead(1, "/Users/danielstahl/Documents/Projects/musique-concrete-i_sounds/short_klang_1.aiff")))

    part1()

    Thread.sleep(5000)
  }
}
