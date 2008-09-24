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

	public static boolean switchTest(char a){
		switch(a){
		case 0xa:
		case 0xc:
		case 0xd:
		case 0x1a:
			return true;
		default:
			return false;
		}
	}
	public static boolean ifTest(char a){
		return a==0xa || a==0xc || a==0xd || a==0x1a;
	}

	final static int WARMUPS = 1000000;
	final static int RUNS = 50000;
	public static void main(String[] args){
		//warmup
		for (int i=0;i<WARMUPS;i++){
			char v = (char) (Math.random()*256*256);
			boolean x = switchTest(v);
			x |= ifTest(v);
		}

		long start = System.currentTimeMillis();

		int w = 12;
		boolean x = false;
		for (int h=RUNS;h>0;h--)
		for (int i=RUNS;i>0;i--)
			for (int j=Integer.MAX_VALUE;j>0;j--){
				x |= switchTest('x');
				x &= (5==w);
			}

		long time = System.currentTimeMillis() - start;

		System.out.println("switch: "+time);

		start = System.currentTimeMillis();

		for (int h=RUNS;h>0;h--)
		for (int i=RUNS;i>0;i--)
			for (int j=Integer.MAX_VALUE;j>0;j--){
				x |= ifTest('x');
				x &= (5==w);
			}

		time = System.currentTimeMillis() - start;

		System.out.println("if: "+time);
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
