package leetcode;

import leetcode.Tree.ListNode;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class LeetCodeEasy {

    public boolean isValid(String s) {

        char l_paren = '(';
        char r_paren = ')';
        char l_brace = '{';
        char r_brace = '}';
        char l_brack = '[';
        char r_brack = ']';

        Stack<Character> test = new Stack<Character>();

        for (int i = 0; i < s.length(); i++) {

            char x = s.charAt(i);
            if (x == l_paren)
                test.push(l_paren);
            else if (x == l_brace) {
                test.push(l_brace);
            } else if (x == l_brack) {
                test.push(l_brack);
            } else if (x == r_paren) {
                if (test.isEmpty())
                    return false;
                if (test.pop() != l_paren)
                    return false;
            } else if (x == r_brace) {
                if (test.isEmpty())
                    return false;
                if (test.pop() != l_brace)
                    return false;
            } else if (x == r_brack) {
                if (test.isEmpty())
                    return false;
                if (test.pop() != l_brack)
                    return false;
            }
        }
        return test.isEmpty();
    }

    public int[] runningSum(int[] nums) {

        int[] op = new int[nums.length];
        op[0] = nums[0];
        // double for loop
        // for (int i = 1; i < nums.length; i++) {
        // int sum = 0;
        // for (int j = 0; j <= i; j++) {
        // sum = sum + nums[j];
        // }
        // op[i] = sum;
        // }

        for (int i = 1; i < nums.length; i++) {

            op[i] = op[i - 1] + nums[i];
        }
        return op;
    }

    public int[] shuffle(int[] nums, int n) {
        int[] op = new int[nums.length];

        for (int i = 0, j = 0; j < n; j++) {

            op[i] = nums[j];
            op[i + 1] = nums[n + j];
            i = i + 2;

        }

        return op;
    }

    public List<Boolean> kidsWithCandies(int[] candies, int extraCandies) {

        List<Boolean> isMax = new ArrayList<Boolean>();
        int max = 0;
        for (int i = 0; i < candies.length; i++) {
            if (max <= candies[i])
                max = candies[i];
        }

        for (int i = 0; i < candies.length; i++) {
            if (candies[i] + extraCandies >= max) {
                isMax.add(true);
            } else {
                isMax.add(false);
            }
        }
        return isMax;
    }

    public int numIdenticalPairs(int[] nums) {

        int count = 0;
        for (int i = 0; i < nums.length; i++) {
            for (int j = i + 1; j < nums.length; j++) {
                if (nums[i] - nums[j] == 0)
                    count++;
            }
        }
        return count;
    }

    public int fastnumIdenticalPairs(int[] nums) {

        int count = 0;
        Map<Integer, Integer> map = new HashMap<Integer, Integer>();
        for (int i = 0; i < nums.length; i++) {
            if (map.containsKey(nums[i])) {
                count += map.get(nums[i]);
                map.replace(nums[i], map.get(nums[i]) + 1);
            } else {
                map.put(nums[i], 1);
            }
        }
        return count;
    }

    public String defangIPaddr(String address) {

        return address.replaceAll("\\.", "[.]");
    }

    public int numJewelsInStones(String J, String S) {

        Map<Character, Integer> map = new HashMap<>();
        for (char c : S.toCharArray()) {

            if (map.containsKey(c)) {
                int count = map.get(c) + 1;
                map.put(c, count);
            } else {
                map.put(c, 1);
            }
        }
        int count = 0;
        for (char c : J.toCharArray()) {

            if (map.containsKey(c)) {
                count = count + map.get(c);
            }
        }
        return count;
    }

    public int numberOfSteps(int num) {

        int count = 0;
        return numberOfSteps(num, count);
    }

    private int numberOfSteps(int num, int count) {

        if (num == 0) {
            return count;
        }
        int remainder = num % 2;
        if (remainder == 0) {
            num = num / 2;
            count = count + 1;
        } else {
            num = (num - 1);
            count = count + 1;
        }
        return numberOfSteps(num, count);
    }

    public String restoreString(String s, int[] indices) {

        int pointer = 0;
        String op = "";
        for (int i = 0; i < indices.length; i++) {

            for (int j = 0; j < indices.length; j++) {

                if (indices[j] == pointer) {
                    op = op + s.charAt(j);
                    pointer = pointer + 1;
                    break;
                }
            }

        }
        return op;
    }

    public String restoreStringfast(String s, int[] indices) {

        String op = "";
        Map<Integer, Character> map = new HashMap<>();
        for (int i : indices) {
            map.put(indices[i], s.charAt(i));
        }

        for (int i = 0; i < indices.length; i++) {
            op = op + map.get(i);
        }
        return op;
    }

    public int[] smallerNumbersThanCurrent(int[] nums) {

        int[] op = new int[nums.length];
        for (int i = 0; i < nums.length; i++) {
            int count = 0;
            for (int j = 0; j < op.length; j++) {

                if (j != i) {
                    if (nums[i] > nums[j]) {
                        count = count + 1;
                    }
                }

            }
            op[i] = count;
        }
        return op;
    }

    public int subtractProductAndSum(int n) {

        int product = 1;
        int sum = 0;
        int remainder = 0;
        while (n >= 10) {

            remainder = n % 10;

            product = product * remainder;
            sum = sum + remainder;
            n = n / 10;
        }
        product = product * n;
        sum = sum + n;
        return product - sum;
    }

    public int[] decompressRLElist(int[] nums) {
        int length = 0;
        for (int i = 0; i < nums.length; i = i + 2) {
            length = length + nums[i];
        }
        int[] op = new int[length];
        int x = 0;
        for (int i = 0; i < nums.length; i = i + 2) {
            int len = nums[i];

            for (int j = x; j < len + x; j++) {
                op[j] = nums[i + 1];

            }
            x = x + nums[i];

        }
        return op;
    }

    public int[] createTargetArray(int[] nums, int[] index) {

        int[] op = new int[nums.length];
        boolean[] p = new boolean[nums.length];

        for (int i = 0; i < nums.length; i++) {

            int ind = index[i];
            if (p[ind]) {

                int temp = nums[i];
                for (int j = nums.length - 1; j > ind; j--) {

                    op[j] = op[j - 1];
                    p[j] = true;
                }
                op[ind] = temp;
            } else {
                op[ind] = nums[i];
                p[ind] = true;
            }
        }
        return op;
    }

    public int balancedStringSplit(String s) {

        int countL = 0;
        int countR = 0;
        int total = 0;
        // for (int i = 0; i < s.length(); i++) {

        for (int j = 0; j < s.length(); j++) {

            if (s.charAt(j) == 'L') {
                countL = countL + 1;
            }
            if (s.charAt(j) == 'R') {
                countR = countR + 1;
            }
            if (countL == countR) {
                total = total + 1;
                // s = s.substring(j+1);
            }

            // }

        }
        return total;

    }

    public int rangeSumBST(TreeNode root, int L, int R) {

        Stack<TreeNode> queue = new Stack();
        queue.add(root);
        int count = 0;
        while (!queue.isEmpty()) {

            TreeNode temp = queue.pop();
            if (temp != null) {
                if (L <= temp.value && temp.value <= R) {
                    count = count + temp.value;
                }
                if (L < temp.value) {
                    queue.push(temp.left);
                }
                if (L < temp.value) {
                    queue.push(temp.right);
                }

            }
        }
        return count;
    }

    public int getDecimalValue(ListNodeOld head) {
        int count = 0;
        int op = 0;
        ListNodeOld temp = head;
        while (temp != null) {
            // op = (int) (op + head.val * Math.pow(2, count));
            count++;
            temp = temp.next;

        }

        System.out.println(count);
        while (head != null) {
            op = (int) (op + head.val * Math.pow(2, count - 1));
            count--;
            head = head.next;

        }
        return op;
    }

    public int getDecimalValuefast(ListNodeOld head) {
        int num = head.val;
        while (head.next != null) {
            num = num * 2 + head.next.val;
            head = head.next;
        }

        return num;

    }

    public int sumOddLengthSubarrays(int[] arr) {

        int sum = 0;
        for (int i = 1; i <= arr.length; i++) {

            if (!((i % 2) == 0)) {

                for (int j = 0; j <= arr.length - i; j++) {

                    for (int j2 = j; j2 < i + j; j2++) {
                        sum = sum + arr[j2];
                    }
                }
            }
        }

        return sum;
    }

    public int findNumbers(int[] nums) {

        int sum = 0;
        int temp = 0;
        for (int i = 0; i < nums.length; i++) {
            temp = numofInteger(nums[i]);
            if (temp % 2 == 0) {
                sum += 1;
            }
        }

        return sum;
    }

    public int numofInteger(int num) {

        int count = 0;
        while (num != 0) {

            count += 1;
            num = num / 10;
        }
        return count;
    }

    public String removeDuplicates(String s) {

        String temp = "";
        Stack<Character> stack = new Stack<Character>();
        stack.push(s.charAt(0));
        for (int i = 1; i < s.length(); i++) {
            if (!stack.isEmpty() && stack.peek() == s.charAt(i)) {
                stack.pop();
            } else {
                stack.push(s.charAt(i));
            }

        }

        for (Character character : stack) {
            temp = temp + character;
        }
        return temp;
    }

    public List<List<Integer>> subsets(int[] nums, List<List<Integer>> lst) {

        if (nums.length == 0) {
            return lst;
        }

        for (int i = 0; i < nums.length; i++) {
            // System.out.print(nums[i] + " ");
            List<Integer> ls = new ArrayList<>();
            ls.add(nums[i]);
            int[] temp = new int[nums.length - i - 1];
            for (int j = i; j < temp.length; j++) {
                temp[j] = nums[j + 1];
            }

            lst.add(ls);
            subsets(temp, lst);

        }

        return lst;
    }

    public void combinations(String str, String suffix) {

        System.out.println(suffix);
        for (int i = 0; i < str.length(); i++) {

            // suffix = suffix+str.substring(0,i);
            // System.out.println(suffix+" "+str.substring(i));
            combinations(str.substring(i + 1), suffix + str.charAt(i));
        }

    }

    public boolean hasCycle(ListNode head) {

        ListNode first = head;
        ListNode second = head.next();
        while (second != first) {

            if (first.next() == null && second.next() == null) {
                return false;
            }
            first = first.next();
            second = second.next();
        }

        return true;

    }

    public void mergeList(int[] arr1, int[] arr2) {

        int i = 0;
        int j = 0;
        int idx = 0;
        int[] op = new int[arr1.length + arr2.length];

        while (i < arr1.length && j < arr2.length) {

            int x = arr1[i];
            int y = arr2[j];
            if (x < y) {
                op[idx] = x;
                i++;
            } else {
                op[idx] = y;
                j++;
            }
            idx++;
        }
        System.out.println(idx+","+i+","+j);

        while (i < arr1.length) {
            op[idx] = arr1[i];
            idx++;
            i++;
        }

        while (j < arr2.length) {
            op[idx] = arr2[j];
            idx++;
            j++;
        }

        for (int a = 0; a < op.length; a++) {
            System.out.println(op[a]);
        }

    }

    public static void main(String[] args) {

        LeetCodeEasy code = new LeetCodeEasy();

        int[] arr1 = {1, 3, 5, 7, 9};
        int[] arr2 = {2, 4, 6, 8, 10, 12, 14};
        code.mergeList(arr1, arr2);

        // int[] nums = { 1, 2, 3, 1, 1, 3 };
        // int[] op = code.runningSum(nums);
        // int[] op = code.shuffle(nums, 4);
        // List<Boolean> op = code.kidsWithCandies(nums, 1);
        // int op = code.fastnumIdenticalPairs(nums);

        // String address = "255.100.50.0";
        // String op = code.defangIPaddr(address);
        // System.out.println(op);
        // int count = code.numJewelsInStones("z", "ZZ");
        // System.out.println(count);

        // int steps = code.numberOfSteps(8);
        // System.out.println(steps);

        // String s = "codeleet";
        // int[] indices = { 4, 5, 6, 7, 0, 2, 1, 3 };
        // String op = code.restoreStringfast(s, indices);
        // System.out.println(op);
        // int[] nums = { 8, 1, 2, 2, 3 };
        // int[] op = code.smallerNumbersThanCurrent(nums);
        // for (int i = 0; i < op.length; i++) {
        // System.out.println(op[i]);
        // }

        // int op = code.subtractProductAndSum(4421);
        // System.out.println(op);
        // int[] nums = { 4, 2, 4, 3, 2 };
        // // int[] op = code.decompressRLElist(nums);
        // int[] index = { 0, 0, 1, 3, 1 };
        // int[] op = code.createTargetArray(nums, index);
        // for (int i = 0; i < op.length; i++) {
        // System.out.print(op[i] + ",");
        // }

        // String s = "RLRRRLLRLL";
        // int op = code.balancedStringSplit(s);
        // System.out.println(op);
        // ListNode head = new ListNode(1, new ListNode(0, new ListNode(1,
        // null)));
        // int op = code.getDecimalValuefast(head);
//		int[] arr = { 1, 2, 3 };
        // int op = code.sumOddLengthSubarrays(arr);
        // System.out.println(code.findNumbers(arr));
        // String op = code.removeDuplicates("aaaaaaaa");
        // System.out.println(op);
//		List<List<Integer>> temp = new ArrayList<>();
//		List<List<Integer>> op = code.subsets(arr, temp);
//		for (List<Integer> list : op) {
//			System.out.println(list.toString());
//		}

        // code.combinations("abc", "");

    }
}
