package sefs

import effect._

package process {
  trait PS {
    private[process] val process: ProcessInternal
  }
}

package object process extends AIOKind[process.PS] {

}