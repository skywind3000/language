import math

from direct.showbase.ShowBase import ShowBase

class MyApp (ShowBase):

    def __init__ (self):
        ShowBase.__init__(self)
        self.scene = self.loader.loadModel('models/environment')
        self.scene.reparentTo(self.render)
        self.scene.setScale(0.25, 0.25, 0.25)
        self.scene.setPos(-8, 42, 0)
        self.taskMgr.add(self.spinCameraTask, 'SpinCameraTask')

    def spinCameraTask (self, task):
        angleDegrees = task.time * 6.0
        angleRadians = angleDegrees * (math.pi / 180.0)
        self.camera.setPos(20 * math.sin(angleRadians), -20.0 * math.cos(angleRadians), 3)
        self.camera.setHpr(angleDegrees, 0, 0)
        return task.cont

app = MyApp()
app.run()



