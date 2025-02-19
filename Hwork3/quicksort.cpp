#include <iostream>
#include <stack>

using namespace std;

// Function to partition the array and place the pivot in the correct position
int partition(int arr[], int low, int high) {
    int pivot = arr[high]; // Choosing last element as pivot
    int i = low - 1;       // Index of smaller elements

    for (int j = low; j < high; j++) {
        if (arr[j] < pivot) { // If current element is smaller than pivot
            i++;
            swap(arr[i], arr[j]); // Swap elements
        }
    }
    swap(arr[i + 1], arr[high]); // Move pivot to correct position
    return (i + 1);
}

// Iterative QuickSort function using a stack
void quickSortIterative(int arr[], int n) {
    stack<pair<int, int>> stk; // Stack to store array slice (low, high)
    stk.push({0, n - 1});      // Push full array range to stack

    while (!stk.empty()) {
        int low = stk.top().first;
        int high = stk.top().second;
        stk.pop();

        int pivot = partition(arr, low, high); // Get pivot index

        // Push left subarray if it exists
        if (pivot - 1 > low) {
            stk.push({low, pivot - 1});
        }

        // Push right subarray if it exists
        if (pivot + 1 < high) {
            stk.push({pivot + 1, high});
        }
    }
}

// Helper function to print an array
void printArray(int arr[], int size) {
    for (int i = 0; i < size; i++) {
        cout << arr[i] << " ";
    }
    cout << endl;
}

// Main function to test QuickSort
int main() {
    int arr[] = {10, 7, 8, 9, 1, 5};
    int n = sizeof(arr) / sizeof(arr[0]);

    cout << "Original array: ";
    printArray(arr, n);

    quickSortIterative(arr, n);

    cout << "Sorted array: ";
    printArray(arr, n);
    return 0;
}
