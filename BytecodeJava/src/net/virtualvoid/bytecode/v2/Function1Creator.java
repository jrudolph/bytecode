package net.virtualvoid.bytecode.v2;

public class Function1Creator extends net.virtualvoid.bytecode.v2.AbstractFunction1{
	public Object apply(Object o){
		return ((Integer)o) + 5;
	}

	private int succ(int i){
		return i + 115;
	}
}
