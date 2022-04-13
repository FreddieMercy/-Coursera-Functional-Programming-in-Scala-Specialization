package scalashop

import org.scalameter.*

import java.util.concurrent.*
import scala.collection.mutable
import scala.collection.mutable.Set
import scala.util.DynamicVariable

/** The value of every pixel is represented as a 32 bit integer. */
type RGBA = Int

/** Returns the red component. */
def red(c: RGBA): Int = (0xff000000 & c) >>> 24

/** Returns the green component. */
def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

/** Returns the blue component. */
def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

/** Returns the alpha component. */
def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

/** Used to create an RGBA value from separate components. */
def rgba(r: Int, g: Int, b: Int, a: Int): RGBA =
	(r << 24) | (g << 16) | (b << 8) | (a << 0)

/** Restricts the integer into the specified range. */
def clamp(v: Int, min: Int, max: Int): Int =
	if v < min then min
	else if v > max then max
	else v

/** Image is a two-dimensional matrix of pixel values. */
class Img(val width: Int, val height: Int, private val data: Array[RGBA]):
	def this(w: Int, h: Int) = this(w, h, new Array(w * h))
	
	def apply(x: Int, y: Int): RGBA = data(y * width + x)
	
	def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c

/** Computes the blurred RGBA value of a single pixel of the input image. */
def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
	
	def twoWhilteLoopImplementation_GetPixels(x: Int, y: Int, width: Int, height: Int) = {
		var i = x
		var j = y
		
		var a: Int = 0
		var r: Int = 0
		var g: Int = 0
		var b: Int = 0
		
		val size: Int = (width - x) * (height - y)
		
		while (i < width) {
			while (j < height) {
				
				a += alpha(src(i, j))
				r += red(src(i, j))
				g += green(src(i, j))
				b += blue(src(i, j))
				
				j += 1
			}
			j = y
			i += 1
		}
		
		rgba(r / size, g / size, b / size, a / size)
	}
	
	twoWhilteLoopImplementation_GetPixels(clamp(src.width - radius, 0, src.width),
		clamp(src.height - radius, 0, src.height),
		clamp(src.width + radius, 0, src.width),
		clamp(src.height + radius, 0, src.width))
}

}

val forkJoinPool = ForkJoinPool()

abstract class TaskScheduler:
	def schedule[T](body: => T): ForkJoinTask[T]
	
	def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
		val right = task {
			taskB
		}
		val left = taskA
		(left, right.join())

class DefaultTaskScheduler extends TaskScheduler :
	def schedule[T](body: => T): ForkJoinTask[T] =
		val t = new RecursiveTask[T] {
			def compute = body
		}
		Thread.currentThread match
			case wt: ForkJoinWorkerThread =>
				t.fork()
			case _ =>
				forkJoinPool.execute(t)
		t

val scheduler =
	DynamicVariable[TaskScheduler](DefaultTaskScheduler())

def task[T](body: => T): ForkJoinTask[T] =
	scheduler.value.schedule(body)

def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
	scheduler.value.parallel(taskA, taskB)

def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) =
	val ta = task {
		taskA
	}
	val tb = task {
		taskB
	}
	val tc = task {
		taskC
	}
	val td = taskD
	(ta.join(), tb.join(), tc.join(), td)
