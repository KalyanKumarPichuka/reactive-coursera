package week2.simulation

/**
 * Easy mixin trait to attach to our Gates abstract class
 */
trait Parameters {
  def InverterDelay = 2
  def AndGateDelay = 3
  def OrGateDelay = 5
}
