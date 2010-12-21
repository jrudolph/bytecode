package net.virtualvoid.bytecode
package backend

import java.lang.{ String => jString }

/** This trait contains the methods which form the contract between Mnemonic's
 *  instructions and the backend implementations. Each backend has to implement
 *  its own frame type which gets called by the instruction implementation.
 */
trait BackendSupport[+ST <: List] {
    def depth = -1
    def frame = this
    
    def stack:ST

    def bipush[ST2>:ST<:List](i1:Int):F[ST2**Int]
    def ldc[ST2>:ST<:List](str:jString):F[ST2**jString]
    
    def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int]
    def isub_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int]
    def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int]
    def pop_int[R<:List](rest:R):F[R]
    def dup_int[R<:List,T](rest:R,top:T):F[R**T**T]
    def swap_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2]
    def dup_x1_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1]

    def invokemethod[R<:List,U](handle:MethodHandle):F[R**U]
    def invokeconstructor[R<:List,U](cons: Constructor): F[R**U]
    def new_int[R <: List, U](cl: Class[U]): F[R**U]
                                   
    def getstatic_int[ST2>:ST<:List,T](code:scala.reflect.Code[()=>T]):F[ST2**T]
    def putstatic_int[R<:List,T](rest:R,top:T,code:scala.reflect.Code[T=>Unit]):F[R]
    
    def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U]
    def conditional[R<:List,T,ST2<:List](cond:Int,rest:R,top:T
    									,thenB:F[R]=>F[ST2]
    									,elseB:F[R]=>F[ST2]):F[ST2]

    def aload_int[R<:List,T](rest:R,array:AnyRef/*Array[T]*/,i:Int):F[R**T]
    def astore_int[R<:List,T](rest:R,array:AnyRef,index:Int,t:T):F[R]
    def arraylength_int[R<:List](rest:R,array:AnyRef):F[R**Int]
    
    def tailRecursive_int[ST1>:ST<:List,ST2<:List]
        (func: (F[ST1] => F[ST2]) => (F[ST1]=>F[ST2]))
        	(fr:F[ST1]):F[ST2]
    
    def pop_unit_int[R<:List](rest:R):F[R]

    def newInstance[T,ST2>:ST<:List](cl:Class[T]):F[ST2**T]
    
    def withLocal_int[T,ST<:List,ST2<:List](top:T,rest:ST,code:Local[T]=>F[ST]=>F[ST2]):F[ST2]
    
    def withTargetHere_int[X,ST2>:ST<:List](code:Target[ST2] => F[ST2] => X):X
    
    def conditionalImperative[R<:List,T,ST2<:List](cond:Int,rest:R,top:T,thenB:F[R]=>Nothing):F[R]

    def lookupSwitch[R <: List, ST2 <: List](cond: Int, rest: R)(candidates: Int*)(mapping: F[R] => PartialFunction[Option[Int], F[ST2]]): F[ST2]
}
