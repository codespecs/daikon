package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

public class SplitterList4Dsaa {

	static {

		SplitterList.put("ShellSort", new Splitter[] {
			new ShellSortSplitter_sort_0(),
			new ShellSortSplitter_sort_1(),
			new ShellSortSplitter_sort_2()});

		SplitterList.put("LinkedListItr", new Splitter[] {
			new LinkedListItrSplitter_advance()});

		SplitterList.put("InsertionSort", new Splitter[] {
			new InsertionSortSplitter_sort1_0(),
			new InsertionSortSplitter_sort1_1(),
			new InsertionSortSplitter_sort_0(),
			new InsertionSortSplitter_sort_1()});

		SplitterList.put("Sample11", new Splitter[] {
			new Sample11Splitter_main(),
			new Sample11Splitter_test_0(),
			new Sample11Splitter_test_1()});

		SplitterList.put("QuickSort", new Splitter[] {
			new QuickSortSplitter_median3_0(),
			new QuickSortSplitter_median3_1(),
			new QuickSortSplitter_median3_2(),
			new QuickSortSplitter_sort_0(),
			new QuickSortSplitter_sort_1(),
			new QuickSortSplitter_sort_2(),
			new QuickSortSplitter_sort_3()});

		SplitterList.put("Sample12", new Splitter[] {
			new Sample12Splitter_main(),
			new Sample12Splitter_test_0(),
			new Sample12Splitter_test_1()});

		SplitterList.put("HeapSort", new Splitter[] {
			new HeapSortSplitter_percDown_0(),
			new HeapSortSplitter_percDown_1(),
			new HeapSortSplitter_sort_0(),
			new HeapSortSplitter_sort_1(),
			new HeapSortSplitter_sort_2()});

		SplitterList.put("StackAr", new Splitter[] {
			new StackArSplitter_pop(),
			new StackArSplitter_push(),
			new StackArSplitter_top(),
			new StackArSplitter_topAndPop()});

		SplitterList.put("CursorListItr", new Splitter[] {
			new CursorListItrSplitter_advance()});

		SplitterList.put("IntegerList", new Splitter[] {
			new IntegerListSplitter_find(),
			new IntegerListSplitter_insert(),
			new IntegerListSplitter_remove()});

		SplitterList.put("Sample81", new Splitter[] {
			new Sample81Splitter_generateArray(),
			new Sample81Splitter_main(),
			new Sample81Splitter_printArray()});

		SplitterList.put("BinarySearchTree", new Splitter[] {
			new BinarySearchTreeSplitter_find1(),
			new BinarySearchTreeSplitter_findMax1_0(),
			new BinarySearchTreeSplitter_findMax1_1(),
			new BinarySearchTreeSplitter_findMin1_0(),
			new BinarySearchTreeSplitter_findMin1_1(),
			new BinarySearchTreeSplitter_insert1(),
			new BinarySearchTreeSplitter_printTree(),
			new BinarySearchTreeSplitter_printTree1(),
			new BinarySearchTreeSplitter_remove1_0(),
			new BinarySearchTreeSplitter_remove1_1(),
			new BinarySearchTreeSplitter_removeMin_0(),
			new BinarySearchTreeSplitter_removeMin_1(),
			new BinarySearchTreeSplitter_size1()});

		SplitterList.put("StackLi", new Splitter[] {
			new StackLiSplitter_pop(),
			new StackLiSplitter_size(),
			new StackLiSplitter_top(),
			new StackLiSplitter_topAndPop()});

		SplitterList.put("Sample82", new Splitter[] {
			new Sample82Splitter_generateArray(),
			new Sample82Splitter_main(),
			new Sample82Splitter_printArray()});

		SplitterList.put("Sample83", new Splitter[] {
			new Sample83Splitter_generateArray(),
			new Sample83Splitter_main(),
			new Sample83Splitter_printArray()});

		SplitterList.put("Sample84", new Splitter[] {
			new Sample84Splitter_generateArray(),
			new Sample84Splitter_main(),
			new Sample84Splitter_printArray()});

		SplitterList.put("Sample85", new Splitter[] {
			new Sample85Splitter_generateArray(),
			new Sample85Splitter_main(),
			new Sample85Splitter_printArray()});

		SplitterList.put("ComparableList", new Splitter[] {
			new ComparableListSplitter_find(),
			new ComparableListSplitter_insert(),
			new ComparableListSplitter_remove()});

		SplitterList.put("Sample1", new Splitter[] {
			new Sample1Splitter_main(),
			new Sample1Splitter_test_0(),
			new Sample1Splitter_test_1()});

		SplitterList.put("LinkedList", new Splitter[] {
			new LinkedListSplitter_insert(),
			new LinkedListSplitter_remove()});

		SplitterList.put("Sample2", new Splitter[] {
			new Sample2Splitter_main(),
			new Sample2Splitter_test_0(),
			new Sample2Splitter_test_1()});

		SplitterList.put("Sample3", new Splitter[] {
			new Sample3Splitter_main(),
			new Sample3Splitter_test_0(),
			new Sample3Splitter_test_1()});

		SplitterList.put("Sample4", new Splitter[] {
			new Sample4Splitter_main(),
			new Sample4Splitter_test_0(),
			new Sample4Splitter_test_1()});

		SplitterList.put("Sample5", new Splitter[] {
			new Sample5Splitter_main(),
			new Sample5Splitter_test_0(),
			new Sample5Splitter_test_1()});

		SplitterList.put("MergeSort", new Splitter[] {
			new MergeSortSplitter_merge_0(),
			new MergeSortSplitter_merge_1(),
			new MergeSortSplitter_merge_2(),
			new MergeSortSplitter_merge_3(),
			new MergeSortSplitter_merge_4(),
			new MergeSortSplitter_sort()});

		SplitterList.put("Sample6", new Splitter[] {
			new Sample6Splitter_main(),
			new Sample6Splitter_test_0(),
			new Sample6Splitter_test_1()});

		SplitterList.put("QueueAr", new Splitter[] {
			new QueueArSplitter_dequeue(),
			new QueueArSplitter_enqueue(),
			new QueueArSplitter_getFront(),
			new QueueArSplitter_makeEmpty()});

		SplitterList.put("CursorList", new Splitter[] {
			new CursorListSplitter_alloc(),
			new CursorListSplitter_insert(),
			new CursorListSplitter_length()});

		SplitterList.put("SortedListItr", new Splitter[] {
			new SortedListItrSplitter_advance()});

	}
}
