namespace MonogameTest

open System
open System
open System
open System.Numerics
open System.Timers
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type Sprite =
    {position: Vector2; speed: float32; texture: Texture2D; size: Point; offset: Point;}
    member this.Draw(spriteBatch: SpriteBatch) =
        let sourceRect = Rectangle(this.offset, this.size)
        spriteBatch.Draw(this.texture, this.position, Nullable.op_Implicit sourceRect, Color.White)
        
        
module AnimationFrames =
    let horizontalStrip(framecount, size: Point, offset: Point) =
        [| for i in 0..framecount-1 ->
            Rectangle(offset + Point(size.X * i, 0) , size)
        |]
        
type Animation =
    {
        frames: Rectangle array
        fps: int
        currentFrame: int
        frameTimer: TimeSpan
        frameLength: TimeSpan
        size: Point
    }
    
    static member Create(framecount, fps, size: Point,offset: Point) =
        let frames = AnimationFrames.horizontalStrip(framecount, size, offset)
        {
            frames = frames
            currentFrame = 0
            frameTimer = TimeSpan.Zero
            frameLength = TimeSpan.FromSeconds (float (1.f / float32 fps))
            fps = fps
            size = size
        }
        
    member this.CurrentFrame =
        this.frames.[this.currentFrame]
        

module Animation =
    let reset anim =
        {anim with 
            currentFrame = 0
            frameTimer = TimeSpan.Zero}
        
    let update (gameTime: GameTime) (anim: Animation) =
        let newFrameTimer, newFrame =
            match anim.frameTimer + gameTime.ElapsedGameTime with
            | n when n >= anim.frameLength ->
                TimeSpan.Zero, (anim.currentFrame + 1) % anim.frames.Length
            | n -> n, anim.currentFrame
            
        {anim with
            frameTimer = newFrameTimer
            currentFrame = newFrame
         }
        
type AnimationKey =
    | IdleLeft
    | IdleRight
    | IdleDown
    | IdleUp
    | WalkLeft
    | WalkRight
    | WalkDown
    | WalkUp
    
type AnimatedSprite =
    {
        texture: Texture2D
        animations: Map<AnimationKey, Animation>
        currentAnimationKey: AnimationKey
        isAnimating: bool
        speed: float32
        position: Vector2
        facingRight: bool
    }
    member this.CurrentAnimation = this.animations.[this.currentAnimationKey]
    member this.Size with get() = this.CurrentAnimation.size
    
module AnimatedSprite =
    let resetAnimation key animatedSprite =
        animatedSprite.animations.[key]
        |> Animation.reset
        
    let updateAnimation key gameTime animatedSprite =
        let animation = animatedSprite.animations.[key]
        if animatedSprite.isAnimating then
            animation |> Animation.update gameTime
        else
            animation
            
    let draw(animSprite: AnimatedSprite) (gameTime: GameTime) (sb: SpriteBatch) =
        let flip =
            if animSprite.facingRight then
                SpriteEffects.None
            else
                SpriteEffects.FlipHorizontally
              
        sb.Draw(animSprite.texture, animSprite.position, Nullable.op_Implicit animSprite.CurrentAnimation.CurrentFrame, Color.White, 0.0f, Vector2.Zero, 1.0f, flip, 1.0f)

[<AutoOpen>]
module MonoGameExtensions =
    type Viewport with 
        member this.Center =
            Vector2(float32 this.Width * 0.5f, float32 this.Height * 0.5f)


type Camera(viewport: Viewport) =
    member val WorldToScreen = Matrix.Identity with get,set
    member val ScreenToWorld = Matrix.Identity with get,set
    member val Zoom = 1.0f with get,set
    member val Position = Vector2.Zero with get,set
    member val Rotation = 0.0f with get,set
    
    member this.Update(pos: Vector2) =
        this.Position <- pos
        this.WorldToScreen <-
            Matrix.CreateTranslation(Vector3(-pos, 0.0f)) *
            Matrix.CreateRotationZ(this.Rotation) *
            Matrix.CreateScale(Vector3(this.Zoom, this.Zoom, 1.f)) *
            Matrix.CreateTranslation( Vector3(viewport.Center, 0.f))
        this.ScreenToWorld <- Matrix.Invert(this.WorldToScreen)
        
        
type TileSet =
    {
        tilesWide: int
        tilesHigh: int
        tileWidth: int
        tileHeight: int
        texture: Texture2D
        sourceRectangles: Rectangle array
    }
    
module TileSet =
    let CreateTileSet(tilesWide, tilesHigh, tileWidth, tileHeight, texture) =
        let sourceRectangles =
            [|
                for y in 0..tilesHigh-1 do
                    for x in 0..tilesWide-1 do
                        yield Rectangle(x*tileWidth, x*tileHeight, tileWidth, tileHeight)
            |]
        
        {
            tilesWide = tilesWide
            tilesHigh = tilesHigh
            tileWidth = tileWidth
            tileHeight = tileHeight
            texture = texture
            sourceRectangles = sourceRectangles
        }
        
type TileLayer = {
    tiles: int array
    width: int
    height: int
    visible: bool
}

module TileLayer =
    let getTileId x y (layer: TileLayer) =
        match x, y with
        | (x,y) when x < 0 || y < 0 -> None
        | (x,y) when x > layer.width || y > layer.height -> None
        | _ ->
            let index = y * layer.width + x
            match layer.tiles |> Array.tryItem index with
            | Some tileId when tileId > 0 -> Some(tileId-1) //id is 1-based, 0 being empty cell
            | _ -> None
            
    let vectorToCell (position: Vector2) (tileset: TileSet) =
        Point(int position.X / tileset.tileWidth, int position.Y / tileset.tileHeight)
        
    let draw (spriteBatch: SpriteBatch, tileset: TileSet, camera: Camera, layer: TileLayer, game: Game) =
        if not layer.visible then ()
        else
            let cameraPoint =
                let location =
                    Vector2(camera.Position.X - (float32 game.GraphicsDevice.Viewport.Width * 0.5f),
                            camera.Position.Y - (float32 game.GraphicsDevice.Viewport.Height * 0.5f))
                vectorToCell location tileset
            
            let viewPoint =
                let location =
                    Vector2(camera.Position.X + (float32 game.GraphicsDevice.Viewport.Width * 0.5f),
                            camera.Position.Y + (float32 game.GraphicsDevice.Viewport.Height * 0.5f))
                vectorToCell location tileset
                
            let minX, minY = max 0 (cameraPoint.X - 1), max 0 (cameraPoint.Y - 1)
            let maxX, maxY = min (viewPoint.X + 1) layer.width - 1, min (viewPoint.Y + 1) layer.height - 1
            
            for y in minY..maxY do
                for x in minX..maxX do
                    match getTileId x y layer with
                    | None -> ()
                    | Some tile ->
                        if tile = -1 then () else
                        let destination = Rectangle(x * tileset.tileWidth, y * tileset.tileHeight, tileset.tileWidth, tileset.tileHeight)
                        spriteBatch.Draw(tileset.texture, destination, Nullable.op_Implicit tileset.sourceRectangles.[tile], Color.White)
            
        

type Game1 () as this =
    inherit Game()
 
    let graphics = new GraphicsDeviceManager(this, PreferredBackBufferWidth = 800, PreferredBackBufferHeight = 600)
    let mutable spriteBatch = Unchecked.defaultof<_>
    let mutable playerSpriteSheet = Unchecked.defaultof<Texture2D>
    let mutable player = Unchecked.defaultof<Sprite>
    let mutable newPlayer = Unchecked.defaultof<AnimatedSprite>
    let mutable playerAnimations = Unchecked.defaultof<_>
    let mutable camera = Unchecked.defaultof<_>
    let mutable tileset = Unchecked.defaultof<TileSet>
    let mutable tilelayer = Unchecked.defaultof<TileLayer>
    let mutable terrain = Unchecked.defaultof<Texture2D>
    
    let (|KeyDown|_|) k (state: KeyboardState) =
        if state.IsKeyDown k then Some() else None
    
    let getMovementVector = function
        | KeyDown Keys.W -> Vector2(0.f, -1.f), WalkUp
        | KeyDown Keys.S -> Vector2(0.f, 1.f), WalkDown
        | KeyDown Keys.A -> Vector2(-1.f, 0.f), WalkLeft
        | KeyDown Keys.D -> Vector2(1.f, 0.f), WalkRight
        | _ -> Vector2.Zero, IdleDown

    do
        this.Content.RootDirectory <- "Content"
        this.IsMouseVisible <- true

    override this.Initialize() =
        // TODO: Add your initialization logic here
        let frameSize = Point(16,28)
        let anims =
            [
                IdleUp, Animation.Create(4,4,frameSize, Point(128,36))
                IdleDown, Animation.Create(4,4,frameSize, Point(128,36))
                IdleLeft, Animation.Create(4,4,frameSize, Point(128,36))
                IdleRight, Animation.Create(4,4,frameSize, Point(128,36))
                WalkUp, Animation.Create(4,8,frameSize, Point(192,36))
                WalkDown, Animation.Create(4,8,frameSize, Point(192,36))
                WalkLeft, Animation.Create(4,8,frameSize, Point(192,36))
                WalkRight, Animation.Create(4,8,frameSize, Point(192,36))
            ] |> Map.ofList
            
        playerAnimations <- anims    
        terrain <- this.Content.Load<Texture2D>("DungeonTileset")
        tileset <- TileSet.CreateTileSet (16, 16, 16, 16, terrain)
        camera <- Camera(this.GraphicsDevice.Viewport)
        base.Initialize()

    override this.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        playerSpriteSheet <- this.Content.Load<Texture2D>("DungeonTileset")
        
        newPlayer <- {
            texture = playerSpriteSheet
            animations = playerAnimations
            currentAnimationKey = AnimationKey.IdleDown
            isAnimating = false
            speed = 166.f
            position = Vector2.Zero
            facingRight = true
        }

        // TODO: use this.Content to load your game content here
 
    override this.Update (gameTime) =
        if (GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape))
        then this.Exit();

        let walkingToIdle = function
            | WalkUp -> IdleUp
            | WalkDown -> IdleDown
            | WalkLeft -> IdleLeft
            | WalkRight -> IdleRight
            | otherIdle -> otherIdle
        
        // TODO: Add your update logic here
        let movementVector, isMoving, animationKey =
            let movementVector, animationKey = getMovementVector(Keyboard.GetState())
            if movementVector = Vector2.Zero then
                movementVector, false, walkingToIdle newPlayer.currentAnimationKey  
            else movementVector |> Vector2.Normalize, true, animationKey
        
        let newPosition player =
            let newPos =
                player.position + movementVector * player.speed * float32 gameTime.ElapsedGameTime.TotalSeconds
            
            let playerSize = player.Size.ToVector2()
            
            let minClamp = Vector2.Zero - playerSize*0.5f
            let maxClamp = Vector2(float32 this.GraphicsDevice.Viewport.Width,
                                   float32 this.GraphicsDevice.Viewport.Height) - playerSize*0.5f
            
            Vector2.Clamp(newPos, minClamp, maxClamp)
        
        
        let newAnimation =
            if newPlayer.currentAnimationKey = animationKey then
                newPlayer |> AnimatedSprite.updateAnimation animationKey gameTime
            else
                newPlayer |> AnimatedSprite.resetAnimation animationKey
               
        let facingRight =
            if animationKey = AnimationKey.WalkLeft then
                false
            elif animationKey = AnimationKey.WalkRight then
                true
            else
                newPlayer.facingRight
                
        newPlayer <- { newPlayer with
                        position = newPosition newPlayer
                        isAnimating = true
                        currentAnimationKey = animationKey
                        facingRight = facingRight
                        animations = newPlayer.animations |> Map.add animationKey newAnimation }
        
        camera.Update newPlayer.position
    
        base.Update(gameTime)
 
    override this.Draw (gameTime) =
        this.GraphicsDevice.Clear Color.CornflowerBlue

        // TODO: Add your drawing code here
        spriteBatch.Begin(transformMatrix = Nullable.op_Implicit camera.WorldToScreen)
        //player.Draw(spriteBatch)
        AnimatedSprite.draw newPlayer gameTime spriteBatch
        spriteBatch.End()
        base.Draw(gameTime)

