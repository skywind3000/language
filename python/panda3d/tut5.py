import math

from direct.showbase.ShowBase import ShowBase
from direct.actor.Actor import Actor
from direct.interval.IntervalGlobal import Sequence
from panda3d.core import Point3

class MyApp (ShowBase):

    def __init__ (self):
        ShowBase.__init__(self)
        self.scene = self.loader.loadModel('models/environment')
        self.scene.reparentTo(self.render)
        self.scene.setScale(0.25, 0.25, 0.25)
        self.scene.setPos(-8, 42, 0)
        self.taskMgr.add(self.spinCameraTask, 'SpinCameraTask')
        self.pandaActor = Actor('models/panda-model', {'walk': 'models/panda-walk4'})
        self.pandaActor.setScale(0.005, 0.005, 0.005)
        self.pandaActor.reparentTo(self.render)
        self.pandaActor.loop('walk')
        posInterval1 = self.pandaActor.posInterval(13, Point3(0, -10, 0), startPos = Point3(0, 10, 0))
        posInterval2 = self.pandaActor.posInterval(13, Point3(0, 10, 0), startPos = Point3(0, -10, 0))
        hprInterval1 = self.pandaActor.hprInterval(3, Point3(180, 0, 0), startHpr = Point3(0, 0, 0))
        hprInterval2 = self.pandaActor.hprInterval(3, Point3(0, 0, 0), startHpr = Point3(180, 0, 0))
        self.pandaPace = Sequence(posInterval1, hprInterval1, 
                                  posInterval2, hprInterval2, 
                                  name = 'pandaPace')
        self.pandaPace.loop()

    def spinCameraTask (self, task):
        angleDegrees = task.time * 6.0
        angleRadians = angleDegrees * (math.pi / 180.0)
        self.camera.setPos(20 * math.sin(angleRadians), -20.0 * math.cos(angleRadians), 3)
        self.camera.setHpr(angleDegrees, 0, 0)
        return task.cont

app = MyApp()
app.run()





