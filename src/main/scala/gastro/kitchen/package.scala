package gastro

import gastro.kitchen.food.Product

package object kitchen {

  class ProtocolMessage

  // objective : use and abuse of case class

  case class NewMenuMessage(n: Integer) extends ProtocolMessage

  case class NewMenuResponse(m: Option[QuantifiedMenu]) extends ProtocolMessage

  case class CoreProductMessage() extends ProtocolMessage

  case class CoreProductResponse(p: Option[Product]) extends ProtocolMessage

  case class OtherIngredientMessage(m: Menu) extends ProtocolMessage

  case class OtherIngredientResponse(p: Option[Product]) extends ProtocolMessage

  case class BestProductMessage(m: Menu, lp: Seq[Product]) extends ProtocolMessage

  case class BestProductResponse(r: Option[(Double, Product)]) extends ProtocolMessage

  case class QuantityMessage(m: Menu) extends ProtocolMessage

  case class QuantityResponse(m: QuantifiedMenu) extends ProtocolMessage


}


