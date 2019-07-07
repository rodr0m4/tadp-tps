package musiquita
import javax.sound.midi.{MidiChannel, MidiSystem}

object AudioPlayer {
  private val VOLUME = 80
  require(VOLUME <= 127)
  require(VOLUME >= 0)

  def reproducirNotas(notas: List[Nota]): Unit = {
    val melodia = notas.map(nota ⇒ Sonido(Tono(6, nota), Negra))

    reproducir(melodia)
  }

  def reproducir(melodia: Melodia): Unit = {
    val synth = MidiSystem.getSynthesizer

    synth.open()

    val channel = synth.getChannels().head

    melodia.foreach(tocar(channel))

    synth.close()
  }

  def tocar(channel: MidiChannel)(tocable: Tocable) = {
    tocable match {
      case Sonido(tono, figura) ⇒ {
        prenderTono(channel)(tono)

        descansar(figura.duracion)

        apagarTono(channel)(tono)
      }
      case Silencio(figura) ⇒ descansar(figura.duracion)
      case Acorde(tonos, figura) ⇒ {
        tonos.foreach(prenderTono(channel))

        descansar(figura.duracion)

        tonos.foreach(apagarTono(channel))
      }
    }
  }

  def descansar(duration: Int): Unit = {
    Thread.sleep(duration)
  }

  def prenderTono(channel: MidiChannel)(tono: Tono): Unit = {
    channel.noteOn(midiId(tono), VOLUME)
  }

  def apagarTono(channel: MidiChannel)(tono: Tono): Unit = {
    channel.noteOff(midiId(tono))
  }

  private def midiId(tono: Tono) = {
    val id = Nota.notaToId(tono.nota)
    id + 12 * tono.octava + 12
  }
}