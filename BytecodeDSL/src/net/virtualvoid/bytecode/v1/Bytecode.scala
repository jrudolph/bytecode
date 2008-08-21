package net.virtualvoid.bytecode.v1

trait Bytecode {
  trait Stack{}
  trait Empty extends Stack{}
  trait S[T,R<:Stack] extends Stack{}

  trait Local{}
  trait NoLocal extends Local{}
  trait L[T,R<:Local] extends Local{}

  /* A Frame state, consisting of stack and locals*/
  trait F[+StackT<:Stack,+LocalT<:Local]{
    def iadd[R<:Stack,LT<:Local](st:F[S[int,S[int,R]],LT]):F[S[int,R],LT]
  }
  
  object Implicits {
    trait WithStackOps[R<:Stack,L<:Local]{
      def ldc(str:String):F[S[String,R],L]
    } 	 
    trait Int2Stack[R<:Stack,L<:Local]{
      def iadd():F[S[int,R],L]
    }
    trait NotEmptyStack[R<:Stack,L<:Local] extends WithStackOps[R,L]{
      def pop():F[R,L]
    }
    
    implicit def int2Stack[R<:Stack,LT<:Local](st:F[S[int,S[int,R]],LT]):Int2Stack[R,LT] = 
      new Int2Stack[R,LT]{
        def iadd() = st.iadd(st)
      }
    implicit def notEmptyStack[T,R<:Stack,LT<:Local](s:F[S[T,R],LT]):NotEmptyStack[R,LT] = null
    implicit def opsStack[R<:Stack,L<:Local](s:F[R,L]):WithStackOps[R,L] = null
  }
}
