import heapq
class Solution:
    def maxSlidingWindow(self, nums: List[int], k: int) -> List[int]:
        res = []
        heap = []
        high = -1<<31
        for i in range(k):
            n = nums[i]
            heapq.heappush(heap, (-n, -i))
        
        res = [-heap[0][0]]
        N = len(nums)
        for i in range(k, N):
            n = nums[i]
            while heap and -heap[0][1] <= i-k:
                heapq.heappop(heap)
            heapq.heappush(heap, (-n, -i))
            res.append(-heap[0][0])
        
        return res
