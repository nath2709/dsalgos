package leetcode;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import leetcode.Easy.ListNode;


public class LeetCodeMedium {
	public List<List<Integer>> threeSum(int[] nums) {

		Set<List<Integer>> res = new HashSet<List<Integer>>();

		for (int i = 0; i < nums.length; i++) {
			Map<Integer, Integer> map = new HashMap<Integer, Integer>();
			int target = -1 * nums[i];

			for (int j = i + 1; j < nums.length; j++) {
				List<Integer> lst = new ArrayList<Integer>();
				int temp = target - nums[j];
				if (map.containsKey(nums[j])) {
					int key = map.get(nums[j]);
					lst.add(nums[i]);
					lst.add(nums[key]);
					lst.add(nums[j]);
					Collections.sort(lst);
					res.add(lst);

				} else {
					map.put(temp, j);
				}

			}

		}

		return new ArrayList<>(res);

	}

	public int[] nextLargerNodes(ListNode head) {

		ListNode currNode = head;
		int index = 0;
		int counter = 0;
		while (currNode != null) {

			counter++;
			currNode = currNode.next();
		}
		currNode = head;
		int[] arr = new int[counter];

		while (currNode != null) {
			int currVal = currNode.x();
			ListNode nextNode = currNode.next();
			arr[index] = 0;

			while (nextNode != null) {
				int temp = nextNode.x();
				if (currVal < temp) {
					arr[index] = temp;
					break;
				}
				nextNode = nextNode.next();
			}
			currNode = currNode.next();
			index++;

		}
		return arr;

	}

	public List<List<Integer>> allPathsSourceTarget(int[][] graph) {

		List<List<Integer>> res = new ArrayList<List<Integer>>();
		List<Integer> neigh = new ArrayList<Integer>();
		neigh.add(0);
		allpath(graph, 0, neigh, res);
		return res;
	}

	private void allpath(int[][] graph, int index, List<Integer> neigh,
			List<List<Integer>> res) {

		if (index == graph.length - 1) {
			res.add(neigh);
		}

		int[] arr = graph[index];
		for (int i : arr) {
			neigh.add(i);
			allpath(graph, i, neigh, res);
			neigh.remove(neigh.size() - 1);
		}
	}

	public static void main(String[] args) {
		LeetCodeMedium medium = new LeetCodeMedium();

		int[] arr = { -1, 0, 1, 2, -1, -4 };
		List<List<Integer>> res = medium.threeSum(arr);
		// for (List<Integer> list : res) {
		// System.out.println(list);
		// }

		// ListNode x = new ListNode(1,
		// new ListNode(7, new ListNode(5,
		// new ListNode(1, new ListNode(9, new ListNode(2,
		// new ListNode(5, new ListNode(1, null))))))));
		// int[] arr1 = medium.nextLargerNodes(x);
		// for (int i : arr1) {
		// System.out.println(i);
		// }

		int[][] graph = new int[][] { new int[] { 1, 2 }, new int[] { 3 },
				new int[] { 3 }, new int[] {} };
//		List<List<Integer>> res = medium.allPathsSourceTarget(graph);

	}
}
