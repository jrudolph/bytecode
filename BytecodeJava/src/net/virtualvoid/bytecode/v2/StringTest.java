package net.virtualvoid.bytecode.v2;

public class StringTest {
	interface Person{
		String name();
	}
	String format(Person p){
		StringBuilder sb = new StringBuilder();
		sb.append("Name: ");
		sb.append(p.name());
		return sb.toString();
	}
}
