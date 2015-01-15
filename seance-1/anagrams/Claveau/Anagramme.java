import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;


public class Anagramme {
	private ArrayList<ArrayList<Element>> dico;
	
	public Anagramme(File dictionnaire) {
		dico = new ArrayList<ArrayList<Element>>();
		BufferedReader bufferedReader;
		try {
			bufferedReader = new BufferedReader(new FileReader(dictionnaire));
			String s;
			while ((s = bufferedReader.readLine()) != null) {
				while (s.length()>dico.size()) {
					dico.add(new ArrayList<Element>());
				}
				dico.get(s.length()-1).add(new Element(s));
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}
	
	
	
	public void searchAnagramm(String s) {
		Element el = new Element(s);
		System.out.print(s+": ");
		for (Element e : dico.get(s.length()-1)) {
			if (e.equals(el))
				System.out.print(e.getValue()+" ");
		}
		System.out.println();
	}
	
	public static void main(String args[]) {
		Anagramme a = new Anagramme(new File("words"));
		for (String s : args) {
			a.searchAnagramm(s);
		}
	}
	
}
