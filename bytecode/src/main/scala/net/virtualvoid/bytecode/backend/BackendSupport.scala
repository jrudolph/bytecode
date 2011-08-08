package net.virtualvoid.bytecode
package backend

import java.lang.{ String => jString }

/** This trait contains the methods which form the contract between Mnemonic's
 *  instructions and the backend implementations. Each backend has to implement
 *  its own frame type which gets called by the instruction implementation.
 */
trait BackendSupport[+ST <: Stack] {
    def depth = -1
    def frame = this
    
    def stack:ST

    def bipush[ST2>:ST<:Stack](i1:Int):F[ST2**Int]
    def ldc[ST2>:ST<:Stack](str:jString):F[ST2**jString]
    
    def iadd_int[R<:Stack](rest:R,i1:Int,i2:Int):F[R**Int]
    def isub_int[R<:Stack](rest:R,i1:Int,i2:Int):F[R**Int]
    def imul_int[R<:Stack](rest:R,i1:Int,i2:Int):F[R**Int]
    def pop_int[R<:Stack](rest:R):F[R]
    def dup_int[R<:Stack,T](rest:R,top:T):F[R**T**T]
    def swap_int[R<:Stack,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2]
    def dup_x1_int[R<:Stack,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1]

    def invokemethod[R<:Stack,U](handle:MethodHandle):F[R**U]
    def invokeconstructor[R<:Stack,U](cons: Constructor): F[R**U]
    def new_int[ST2 >: ST <: Stack, U](cl: Class[U]): F[ST2**Uninitialized[U]]
                                   
    def getstatic_int[ST2>:ST<:Stack,T](code:scala.reflect.Code[()=>T]):F[ST2**T]
    def putstatic_int[R<:Stack,T](rest:R,top:T,code:scala.reflect.Code[T=>Unit]):F[R]
    
    def checkcast_int[R<:Stack,T,U](rest:R,top:T)(cl:Class[U]):F[R**U]
    def conditional[R<:Stack,T,ST2<:Stack](cond:Int,rest:R,top:T
    									,thenB:F[R]=>F[ST2]
    									,elseB:F[R]=>F[ST2]):F[ST2]

    def aload_int[R<:Stack,T](rest:R,array:AnyRef/*Array[T]*/,i:Int):F[R**T]
    def astore_int[R<:Stack,T](rest:R,array:AnyRef,index:Int,t:T):F[R]
    def arraylength_int[R<:Stack](rest:R,array:AnyRef):F[R**Int]
    
    def tailRecursive_int[ST1>:ST<:Stack,ST2<:Stack]
        (func: (F[ST1] => F[ST2]) => (F[ST1]=>F[ST2]))
        	(fr:F[ST1]):F[ST2]
    
    def pop_unit_int[R<:Stack](rest:R):F[R]

    def newInstance[T,ST2>:ST<:Stack](cl:Class[T]):F[ST2**T]
    
    def withLocal_int[T,ST<:Stack,ST2<:Stack](top:T,rest:ST,code:Local[T]=>F[ST]=>F[ST2]):F[ST2]
    
    def withTargetHere_int[X,ST2>:ST<:Stack](code:Target[ST2] => F[ST2] => X):X
    
    def conditionalImperative[R<:Stack,T,ST2<:Stack](cond:Int,rest:R,top:T,thenB:F[R]=>Nothing):F[R]

    def lookupSwitch[R <: Stack, ST2 <: Stack](cond: Int, rest: R)(candidates: Int*)(mapping: F[R] => PartialFunction[Option[Int], F[ST2]]): F[ST2]
}
