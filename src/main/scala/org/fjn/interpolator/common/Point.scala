package org.fjn.interpolator.common

import java.util.concurrent.ConcurrentMap
import management.ThreadMXBean

/**
 * Created by fjn army of one.
 * User: fran
 * Date: 8/24/12
 * Time: 7:38 PM
 */
case class Point[T <: AnyVal](x: T, y: T, z: T)(implicit m: Manifest[T])

