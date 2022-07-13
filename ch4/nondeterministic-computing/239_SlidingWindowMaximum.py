from collections import deque
class Solution:
    def maxSlidingWindow(self, nums: List[int], k: int) -> List[int]:
        q = deque()
        N = len(nums)
        def clean_deque_and_add(i):
            nonlocal q
            if q and q[0][1] <= i - k:
                q.popleft()
                
            n = nums[i]
            while q and n >= q[-1][0]:
                q.pop()
            
            q.append((n, i))
            
        for i in range(k):
            clean_deque_and_add(i)
        
        res = [q[0][0]]
        for i in range(k, N):
            clean_deque_and_add(i)
            res.append(q[0][0])
        
        return res
            

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
