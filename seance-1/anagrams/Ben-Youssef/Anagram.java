import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collections;
import java.util.Vector;

public class Anagram {

	public static void main(String[] args) throws IOException{
		BufferedReader br = new BufferedReader(new FileReader("../words"));
		Vector<String> words = new Vector<String>();
		String line;
		while((line=br.readLine()) != null){
			words.addElement(line);
		}
		br.close();
		for(String str : args){
			Vector<Character> anagram = new Vector<Character>();
			for(char c : str.toCharArray()){
				anagram.addElement(c);
			}
			Collections.sort(anagram);
			System.out.print(str+" : ");
			for(String word : words){
				Vector<Character> w = new Vector<Character>();
				for(char c : word.toCharArray()){
					w.addElement(c);
				}
				Collections.sort(w);
				if(w.equals(anagram) && !(str.equals(word))){
					System.out.print(word+" ");
				}
			}
			System.out.println();
		}
	}
}
