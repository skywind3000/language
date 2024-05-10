from direct.showbase.ShowBase import ShowBase

class MyApp (ShowBase):

    def __init__ (self):
        ShowBase.__init__(self)
        self.scene = self.loader.loadModel('models/environment')
        self.scene.reparentTo(self.render)
        self.scene.setScale(0.25, 0.25, 0.25)
        self.scene.setPos(-8, 42, 0)
        self.scene.setPos(-8, 42, -5)

app = MyApp()
app.run()


