import chapter11.SampleMonads
import chapter13.{Free, Suspend, Return}

val const42 = Suspend(() => 42)
val flatMap1 = const42.flatMap(v => Return(3*v))
val flatMap2 = flatMap1.flatMap(v => Return(3*v))

Free.runTrampoline(flatMap2)
Free.run(flatMap2)(SampleMonads.function0Monad)()