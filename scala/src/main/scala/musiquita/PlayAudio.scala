package musiquita

import scala.util.Success

object PlayAudio extends App {
  val cumpleaños = "4CM1/4 4C1/4 4D1/2 4C1/4 4F1/2 4E1/2 4C1/8 4C1/4 4D1/2 4C1/2 4G1/2 4F1/2 4C1/8 4C1/4 5C1/2 4A1/2 4F1/8 4F1/4 4E1/2 4D1/2"
  val rickRoll = "4AM1/8 5C1/8 5C#1/8 5C#1/8 5D#1/8 5C1/8 4A#1/8 4G#1/2 - 4A#1/8 4A#1/8 5C1/4 5C#1/8 4A#1/4 4G#1/2 5G#1/4 5G#1/4 5D#1/2"

  val Success((melodíaCumpleaños, _)) = Musiquita.melodia(cumpleaños)
  val Success((melodíaRickRoll, _)) = Musiquita.melodia(rickRoll)

  AudioPlayer.reproducir(melodíaRickRoll)
}