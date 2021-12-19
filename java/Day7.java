import java.io.File;
import java.io.FileNotFoundException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;

class Day7 {
  // inputs before parsing
  private File inputFile;

  // values from input stored here
  private List<Integer> input;

  public static void main(String[] args) {
    Day7 day7 = new Day7();
    day7.part1();
    day7.part2();
  }

  public Day7() {
    // initialize list
    this.input = new ArrayList<>();
    
    // inputs file
    this.inputFile = new File("../inputs/day7");
    readInputs(this.inputFile);
  }

  // solution for part 1 of problem
  public void part1() {
    // i despise mutation of state within methods
    Collections.sort(this.input);

    // median of inputs
    int median = (int) findMedian(this.input);

    // find distance of each input from median
    int distance = findDistance(this.input, median);

    System.out.println(distance);
  }

  public void part2() {
    Collections.sort(this.input);

    int minSum = Integer.MAX_VALUE;

    for (int i = input.get(input.size()-1); i >= input.get(0); i--) {
      int x = findWeightedDistance(this.input, i);
      if (x < minSum) minSum = x;
    }

    System.out.println(minSum);
  }

  // read comma-delimited integers from file
  private void readInputs(File file) {
    // scanner throws exception if file is not found
    try {
      // values are separated by commas
      Scanner scanner = new Scanner(file);
      scanner.useDelimiter(",");

      // read until end of file
      while (scanner.hasNext()) {
        input.add(scanner.nextInt());
      }

      // close scanner after use
      scanner.close();
    } catch (FileNotFoundException e) {
      // fail if file is not found
      System.err.println("Input file not found");
      System.exit(1);
    }
  }

  // find distance from a list of points to one point
  private int findDistance(List<Integer> list, int point) {
    // total distance
    int sum = 0;

    // add each distance to sum
    for (Integer i: list) {
      sum += Math.abs(point - i);
    }

    return sum;
  }
  
  // find median of a list of integers
  private double findMedian(List<Integer> list) {
    // check whether length of list is even or off
    if (list.size() % 2 == 0) {
      // find middle two indexes
      int index1 = ((int) list.size() / 2);
      int index2 = index1 + 1;

      // average of both indexes
      return (list.get(index1) + list.get(index2)) / 2;
    } else {
      // find median element if odd
      int index = ((int) list.size() / 2) + 1;
      return list.get(index);
    }
  }

  private int findWeightedDistance(List<Integer> list, int point) {
    int sum = 0;

    // add each weighted distance to sum
    for (Integer a: list) {
      // weighted distance
      int distance = 0;
      for (int i = 0; i <= Math.abs(point - a); i++) {
        distance += i;
      }

      sum += distance;
    }

    return sum;
  }
}