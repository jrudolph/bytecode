package net.virtualvoid.bytecode.v2;

public class StringTest {
	interface Person{
		String name();
		String[] numbers();
		Iterable<String> numbers_it();
	}
	String format_it(Person p){
		StringBuilder sb = new StringBuilder();
		sb.append("Name: ");
		sb.append(p.name());
		sb.append(" Tel.: ");

		for(String n:p.numbers_it()){
			sb.append(n)
			  .append(",");
		}

		return sb.toString();
	}

	String format(Person p){
		// "Name: #name Tel.: #numbers(,)*"

		StringBuilder sb = new StringBuilder();
		sb.append("Name: ");
		sb.append(p.name());
		sb.append(" Tel.: ");
		String[] numbers = p.numbers();
		int length = numbers.length;
		for(int i=0;i<length;i++){
			sb.append(numbers[i]);
			sb.append(",");
		}

		for(String n:p.numbers()){
			sb.append(n);
			sb.append(",");
		}

		return sb.toString();
	}
}
