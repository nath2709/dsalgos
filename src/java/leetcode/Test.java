package leetcode;

public class Test {

	public static void main(String[] args) {
		int[] temp = { 0, 1, 2, 3 };
		for (int i = temp.length - 1, j = 0; i > 0; i--, j++) {

			temp[i] = temp[i - 1];
		}
		for (int i = 0; i < temp.length; i++) {
			System.out.println(temp[i]);
		}
	}
}
