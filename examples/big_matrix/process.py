import numpy as np
import numpy.random as random
from microcluster_canvas import parallel

@parallel()
async def process():
  a = random.rand(3)
  for _ in range(10):
    b = random.rand(3) * 10
    c = random.rand(3) * 10
    a += b
    # raise Exception("intentional error")
  return a

@parallel()
async def process_2():
  return 1

@parallel()
async def process_3():
  return 2
