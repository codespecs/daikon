package daikon.split.dsaa;

import daikon.split.*;

public class SplitterList4Dsaa {

    static {

	SplitterList.put("ShellSort.sort", new Splitter[] {
	    new ShellSortSplitter_sort_0(),
		new ShellSortSplitter_sort_1(),
		new ShellSortSplitter_sort_2()});

	SplitterList.put("LinkedListItr.advance", new Splitter[] {
	    new LinkedListItrSplitter_advance()});

	SplitterList.put("LinkedListItr.isPastEnd", new Splitter[] {
	    new LinkedListItrSplitter_isPastEnd()});

	SplitterList.put("LinkedListItr.retrieve", new Splitter[] {
	    new LinkedListItrSplitter_retrieve()});

	SplitterList.put("InsertionSort.sort", new Splitter[] {
	    new InsertionSortSplitter_sort1_0(),
		new InsertionSortSplitter_sort1_1(),
		new InsertionSortSplitter_sort_0(),
		new InsertionSortSplitter_sort_1()});

	SplitterList.put("Sample11.main", new Splitter[] {
	    new Sample11Splitter_main()});

	SplitterList.put("Sample11.test", new Splitter[] {
	    new Sample11Splitter_test_0(),
		new Sample11Splitter_test_1()});

	SplitterList.put("QuickSort.median3", new Splitter[] {
	    new QuickSortSplitter_median3_0(),
		new QuickSortSplitter_median3_1(),
		new QuickSortSplitter_median3_2()});

	SplitterList.put("QuickSort.sort", new Splitter[] {
	    new QuickSortSplitter_sort_0(),
		new QuickSortSplitter_sort_1(),
		new QuickSortSplitter_sort_2(),
		new QuickSortSplitter_sort_3()});

	SplitterList.put("Sample12.main", new Splitter[] {
	    new Sample12Splitter_main()});

	SplitterList.put("Sample12.test", new Splitter[] {
	    new Sample12Splitter_test_0(),
	    new Sample12Splitter_test_1()});

	SplitterList.put("HeapSort.percDown", new Splitter[] {
	    new HeapSortSplitter_percDown_0(),
		new HeapSortSplitter_percDown_1()});

	SplitterList.put("HeapSort.sort", new Splitter[] {
	    new HeapSortSplitter_sort_0(),
		new HeapSortSplitter_sort_1(),
		new HeapSortSplitter_sort_2()});

	SplitterList.put("StackAr.pop", new Splitter[] {
	    new StackArSplitter_pop()});

	SplitterList.put("StackAr.push", new Splitter[] {
	    new StackArSplitter_push()});

	SplitterList.put("StackAr.top", new Splitter[] {
	    new StackArSplitter_top()});

	SplitterList.put("StackAr.topAndPop", new Splitter[] {
	    new StackArSplitter_topAndPop()});

	SplitterList.put("StackAr.isEmpty", new Splitter[] {
	    new StackArSplitter_isEmpty()});

	SplitterList.put("StackAr.isFull", new Splitter[] {
	    new StackArSplitter_isFull()});

	SplitterList.put("CursorListItr.advance", new Splitter[] {
	    new CursorListItrSplitter_advance()});

	SplitterList.put("CursorListItr.isPastEnd", new Splitter[] {
	    new CursorListItrSplitter_isPastEnd()});

	SplitterList.put("CursorListItr.retrieve", new Splitter[] {
	    new CursorListItrSplitter_retrieve()});

	SplitterList.put("IntegerList.find", new Splitter[] {
	    new IntegerListSplitter_find()});

	SplitterList.put("IntegerList.insert", new Splitter[] {
	    new IntegerListSplitter_insert()});

	SplitterList.put("IntegerList.remove", new Splitter[] {
	    new IntegerListSplitter_remove()});

	SplitterList.put("Sample81.generateArray", new Splitter[] {
	    new Sample81Splitter_generateArray()});

	SplitterList.put("Sample81.main", new Splitter[] {
	    new Sample81Splitter_main()});

	SplitterList.put("Sample81.printArray", new Splitter[] {
	    new Sample81Splitter_printArray()});

	SplitterList.put("BinarySearchTree.find", new Splitter[] {
	    new BinarySearchTreeSplitter_find1()});

	SplitterList.put("BinarySearchTree.findMax", new Splitter[] {
	    new BinarySearchTreeSplitter_findMax1_0(),
		new BinarySearchTreeSplitter_findMax1_1()});

	SplitterList.put("BinarySearchTree.findMin", new Splitter[] {
	    new BinarySearchTreeSplitter_findMin1_0(),
		new BinarySearchTreeSplitter_findMin1_1()});

	SplitterList.put("BinarySearchTree.insert", new Splitter[] {
	    new BinarySearchTreeSplitter_insert1()});

	SplitterList.put("BinarySearchTree.printTree", new Splitter[] {
	    new BinarySearchTreeSplitter_printTree(),
		new BinarySearchTreeSplitter_printTree1()});

	SplitterList.put("BinarySearchTree.remove", new Splitter[] {
	    new BinarySearchTreeSplitter_remove1_0(),
		new BinarySearchTreeSplitter_remove1_1()});

	SplitterList.put("BinarySearchTree.removeMin", new Splitter[] {
	    new BinarySearchTreeSplitter_removeMin_0(),
		new BinarySearchTreeSplitter_removeMin_1()});

	SplitterList.put("BinarySearchTree.size", new Splitter[] {
	    new BinarySearchTreeSplitter_size1()});

	SplitterList.put("BinarySearchTree.elementAt", new Splitter[] {
	    new BinarySearchTreeSplitter_elementAt()});

	SplitterList.put("BinarySearchTree.isEmpty", new Splitter[] {
	    new BinarySearchTreeSplitter_isEmpty()});

	SplitterList.put("StackLi.pop", new Splitter[] {
	    new StackLiSplitter_pop()});

	SplitterList.put("StackLi.size", new Splitter[] {
	    new StackLiSplitter_size()});

	SplitterList.put("StackLi.top", new Splitter[] {
	    new StackLiSplitter_top()});

	SplitterList.put("StackLi.topAndPop", new Splitter[] {
	    new StackLiSplitter_topAndPop()});

	SplitterList.put("StackLi.isEmpty", new Splitter[] {
	    new StackLiSplitter_isEmpty()});
	
	SplitterList.put("Sample82.generateArray", new Splitter[] {
	    new Sample82Splitter_generateArray()});

	SplitterList.put("Sample82.main", new Splitter[] {
	    new Sample82Splitter_main()});

	SplitterList.put("Sample82.printArray", new Splitter[] {
	    new Sample82Splitter_printArray()});

	SplitterList.put("Sample83.generateArray", new Splitter[] {
	    new Sample83Splitter_generateArray()});

	SplitterList.put("Sample83.main", new Splitter[] {
	    new Sample83Splitter_main()});

	SplitterList.put("Sample83.printArray", new Splitter[] {
	    new Sample83Splitter_printArray()});

	SplitterList.put("Sample84.generateArray", new Splitter[] {
	    new Sample84Splitter_generateArray()});

	SplitterList.put("Sample84.main", new Splitter[] {
	    new Sample84Splitter_main()});

	SplitterList.put("Sample84.printArray", new Splitter[] {
	    new Sample84Splitter_printArray()});

	SplitterList.put("Sample85.generateArray", new Splitter[] {
	    new Sample85Splitter_generateArray()});

	SplitterList.put("Sample85.main", new Splitter[] {
	    new Sample85Splitter_main()});

	SplitterList.put("Sample85.printArray", new Splitter[] {
	    new Sample85Splitter_printArray()});

	SplitterList.put("ComparableList.find", new Splitter[] {
	    new ComparableListSplitter_find()});

	SplitterList.put("ComparableList.insert", new Splitter[] {
	    new ComparableListSplitter_insert()});

	SplitterList.put("ComparableList.remove", new Splitter[] {
	    new ComparableListSplitter_remove()});

	SplitterList.put("Sample1.main", new Splitter[] {
	    new Sample1Splitter_main()});

	SplitterList.put("Sample1.test", new Splitter[] {
	    new Sample1Splitter_test_0(),
		new Sample1Splitter_test_1()});

	SplitterList.put("LinkedList.insert", new Splitter[] {
	    new LinkedListSplitter_insert()});

	SplitterList.put("LinkedList.remove", new Splitter[] {
	    new LinkedListSplitter_remove()});

	SplitterList.put("LinkedList.isEmpty", new Splitter[] {
	    new LinkedListSplitter_isEmpty()});

	SplitterList.put("Sample2.main", new Splitter[] {
	    new Sample2Splitter_main()});

	SplitterList.put("Sample2.test", new Splitter[] {
	    new Sample2Splitter_test_0(),
		new Sample2Splitter_test_1()});

	SplitterList.put("Sample3.main", new Splitter[] {
	    new Sample3Splitter_main()});

	SplitterList.put("Sample3.test", new Splitter[] {
	    new Sample3Splitter_test_0(),
		new Sample3Splitter_test_1()});

	SplitterList.put("Sample4.main", new Splitter[] {
	    new Sample4Splitter_main()});

	SplitterList.put("Sample4.test", new Splitter[] {
	    new Sample4Splitter_test_0(),
		new Sample4Splitter_test_1()});

	SplitterList.put("Sample5.main", new Splitter[] {
	    new Sample5Splitter_main()});

	SplitterList.put("Sample5.test", new Splitter[] {
	    new Sample5Splitter_test_0(),
		new Sample5Splitter_test_1()});

	SplitterList.put("MergeSort.merge", new Splitter[] {
	    new MergeSortSplitter_merge_0(),
		new MergeSortSplitter_merge_1(),
		new MergeSortSplitter_merge_2(),
		new MergeSortSplitter_merge_3(),
		new MergeSortSplitter_merge_4()});

	SplitterList.put("MergeSort.sort", new Splitter[] {
	    new MergeSortSplitter_sort()});

	SplitterList.put("Sample6.main", new Splitter[] {
	    new Sample6Splitter_main()});

	SplitterList.put("Sample6.test", new Splitter[] {
	    new Sample6Splitter_test_0(),
		new Sample6Splitter_test_1()});

	SplitterList.put("QueueAr.dequeue", new Splitter[] {
	    new QueueArSplitter_dequeue()});

	SplitterList.put("QueueAr.enqueue", new Splitter[] {
	    new QueueArSplitter_enqueue()});

	SplitterList.put("QueueAr.getFront", new Splitter[] {
	    new QueueArSplitter_getFront()});

	SplitterList.put("QueueAr.makeEmpty", new Splitter[] {
	    new QueueArSplitter_makeEmpty()});

	SplitterList.put("QueueAr.isEmpty", new Splitter[] {
	    new QueueArSplitter_isEmpty()});

	SplitterList.put("QueueAr.isFull", new Splitter[] {
	    new QueueArSplitter_isFull()});

	SplitterList.put("CursorList.alloc", new Splitter[] {
	    new CursorListSplitter_alloc()});

	SplitterList.put("CursorList.insert", new Splitter[] {
	    new CursorListSplitter_insert()});

	SplitterList.put("CursorList.length", new Splitter[] {
	    new CursorListSplitter_length()});

	SplitterList.put("SortedList.isEmpty", new Splitter[] {
	    new SortedListSplitter_isEmpty()});

	SplitterList.put("SortedListItr.advance", new Splitter[] {
	    new SortedListItrSplitter_advance()});

	SplitterList.put("SortedListItr.isPastEnd", new Splitter[] {
	    new SortedListItrSplitter_isPastEnd()});

	SplitterList.put("OrderedList.insert", new Splitter[] {
	    new OrderedListSplitter_insert()});

	SplitterList.put("OrderedList.remove", new Splitter[] {
	    new OrderedListSplitter_remove()});

	SplitterList.put("OrderedList.isEmpty", new Splitter[] {
	    new OrderedListSplitter_isEmpty()});

    }
}
