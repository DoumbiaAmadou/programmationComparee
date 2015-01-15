import java.util.Arrays;


public class Element {
	private String value;
	private char[] sorted;
	public Element(String s) {
		value = s;
		sorted = sortLetters(s).toCharArray();
	}
	

	public String sortLetters(String s) {
		char[] chars = s.toCharArray();
        Arrays.sort(chars);
        return new String(chars);		
	}
	
	public boolean equals(Object o) {
		if (!(o instanceof Element))
			return false;
		char[] c = ((Element)o).sorted;
		if (c.length != sorted.length)
			return false;
		for (int i =0; i<c.length; i++) {
			if (c[i]!=sorted[i])
				return false;
		}
		return true;	
	}

	public String getValue() {
		return value;
	}

	
}
