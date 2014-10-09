/*
 * Copyright 2007-2014 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb.http

import net.liftweb.common.Full
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.{JsExp, JsCmds, JsCmd}
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.{JsonAST, Printer}
import net.liftweb.util.StringHelpers._
import scala.xml.NodeSeq

trait RoundTripCometActor extends CometActor {
  /**
   * It's the main method to override, to define what is rendered by the CometActor
   *
   * There are implicit conversions for a bunch of stuff to
   * RenderOut (including NodeSeq).  Thus, if you don't declare the return
   * turn to be something other than RenderOut and return something that's
   * coercible into RenderOut, the compiler "does the right thing"(tm) for you.
   * <br/>
   * There are implicit conversions for NodeSeq, so you can return a pile of
   * XML right here.  There's an implicit conversion for NodeSeq => NodeSeq,
   * so you can return a function (e.g., a CssBindFunc) that will convert
   * the defaultHtml to the correct output.  There's an implicit conversion
   * from JsCmd, so you can return a pile of JavaScript that'll be shipped
   * to the browser.<br/>
   * Note that the render method will be called each time a new browser tab
   * is opened to the comet component or the comet component is otherwise
   * accessed during a full page load (this is true if a partialUpdate
   * has occurred.)  You may want to look at the fixedRender method which is
   * only called once and sets up a stable rendering state.
   */
  def render: RenderOut = NodeSeq.Empty

  override def lifespan = Full(LiftRules.clientActorLifespan.vend.apply(this))

  override def hasOuter = false

  override def parentTag = <div style="display: none"/>

  override def lowPriority: PartialFunction[Any, Unit] = {
    case jsCmd: JsCmd =>
      partialUpdate(JsCmds.JsSchedule(JsCmds.JsTry(jsCmd, false)))
    case jsExp: JsExp =>
      partialUpdate(JsCmds.JsSchedule(JsCmds.JsTry(jsExp.cmd, false)))
    case ItemMsg(guid, value) =>
      partialUpdate(JsCmds.JsSchedule(JsRaw(s"lift.sendEvent(${guid.encJs}, {'success': ${Printer.compact(JsonAST.render(value))}} )").cmd))
    case DoneMsg(guid) =>
      partialUpdate(JsCmds.JsSchedule(JsRaw(s"lift.sendEvent(${guid.encJs}, {'done': true} )").cmd))
    case FailMsg(guid, msg) =>
      partialUpdate(JsCmds.JsSchedule(JsRaw(s"lift.sendEvent(${guid.encJs}, {'failure': ${msg.encJs} })").cmd))
    case _ =>

  }
}

object RoundTripCometActor {
  def apply(): RoundTripCometActor = new RoundTripCometActor {}
}

case class ItemMsg(guid: String, item: JValue)
case class DoneMsg(guid: String)
case class FailMsg(guid: String, msg: String)
