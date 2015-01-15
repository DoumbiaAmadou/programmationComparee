import java.io.*;
import java.util.*;

public class Anagram {
	public static final void anagrams(String[] words, HashMap<String, String> dico) {
		for (int i = 0; i < words.length; i++) {
			System.out.print(words[i] + " : ");
			for (Map.Entry<String, String> elementDico : dico.entrySet())
				if (elementDico.getValue().equals(Anagram.sortWord(words[i])) && !elementDico.getKey().equals(words[i]))
					System.out.print(elementDico.getKey() + " ");
			System.out.println();
		}
	}

	public static final String sortWord(String word) {
		char w[] = new char[word.length()];
		for (int i = 0; i < w.length; i++)
			w[i] = word.charAt(i);
		Arrays.sort(w);
		return new String(w);
	}

	public static final HashMap<String, String> hashingDico(File dico) {
		HashMap<String, String> hashDico = new HashMap<String, String>();
		try {
			String word;
			BufferedReader bufferedReader = new BufferedReader(new FileReader(
					dico));
			while ((word = bufferedReader.readLine()) != null)
				hashDico.put(word, Anagram.sortWord(word));
			bufferedReader.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return hashDico;
	}

	public static void main(String[] args) {
		// long before = System.currentTimeMillis();
		HashMap<String, String> dico = Anagram.hashingDico(new File("words"));
		//String[] words = { "niche", "marie", "cabans" };
		Anagram.anagrams(args, dico);
		// long after = System.currentTimeMillis();
		// System.out.println("\n temps d'exécution : " + (after - before) +
		// " ms");
	}
}
