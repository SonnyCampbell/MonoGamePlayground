namespace MonogameTest

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open MonoGame.Extended.Tiled
open MonoGame.Extended.Tiled.Graphics
open MonoGame.Extended



type AABB =
    { halfExtents: Vector2
      center: Vector2 }

    member this.min = Vector2(this.center.X - this.halfExtents.X, this.center.Y - this.halfExtents.Y)
    member this.max = Vector2(this.center.X + this.halfExtents.X, this.center.Y + this.halfExtents.Y)
    member this.size = Vector2(this.halfExtents.X * 2.f, this.halfExtents.Y * 2.f)
    static member create(center, halfExtents) =
        {center = center; halfExtents = halfExtents}

type RigidBody =
    {
        mass: float32
        inverseMass: float32
        aabb: AABB
        velocity: Vector2
    }
    
    static member create(mass, width, height, center, vel) =
        {
            mass = mass
            inverseMass = if mass = 0.f then 0.f else 1.f / mass
            aabb = {center = center; halfExtents = Vector2(width/2.f, height/2.f)}
            velocity = vel
        }
    
type Contact =
    {
        a: RigidBody
        b: RigidBody
        normal: Vector2
        distance: float32
        impulse: float32
    }
    
    static member create(a, b, normal, distance, ?impulse) =
        {
            a = a
            b = b
            normal = normal
            distance = distance
            impulse = Option.defaultValue 0.f impulse
        }
    

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
        facingRight: bool
        jump: float32
        rigidBody: RigidBody
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
              
        sb.Draw(animSprite.texture, animSprite.rigidBody.aabb.min, Nullable.op_Implicit animSprite.CurrentAnimation.CurrentFrame, Color.White, 0.0f, Vector2.Zero, 1.0f, flip, 1.0f)

[<AutoOpen>]
module MonoGameExtensions =
    type Viewport with 
        member this.Center =
            Vector2(float32 this.Width * 0.5f, float32 this.Height * 0.5f)
    
    type Vector2 with
        member this.MajorAxis() =
            if abs this.X > abs this.Y then
                Vector2(float32 (sign this.X), 0.f)
            else
                Vector2(0.f, float32 (sign this.Y))
            
    //Possibly also TiledMapTileLayer ??
    type TiledMapTileset with
        static member tileToWorld x y (tileset: TiledMapTileset) =
            Vector2(float32 (x * tileset.TileWidth), float32 (y * tileset.TileHeight))
        
        static member toAABB x y (tileset: TiledMapTileset) =
            let tileInWorld = TiledMapTileset.tileToWorld x y tileset
            
            let extents = Vector2(float32 tileset.TileWidth / 2.0f, float32 tileset.TileHeight / 2.0f)
            
            AABB.create(tileInWorld + extents, extents)
            
    type TiledMapTileLayer with
        static member getTileId x y (layer: TiledMapTileLayer) =
            match x, y with
            | (x,y) when x < 0 || y < 0 -> None
            | (x,y) when x > layer.Width || y > layer.Height -> None
            | _ ->
                let index = y * layer.Width + x
                match layer.Tiles |> Array.ofSeq |> Array.tryItem index with
                | Some tileId when tileId.GlobalIdentifier > 0 -> Some(tileId.GlobalIdentifier - 1) //id is 1-based, 0 being empty cell
                | _ -> None
        
        static member getBroadphaseTiles (tileLayer: TiledMapTileLayer) (tileset: TiledMapTileset) (min: Vector2) (max: Vector2) =
            let minX = (int min.X) / tileset.TileWidth
            let minY = (int min.Y) / tileset.TileHeight
            
            let maxX = (int (max.X + 0.5f) / tileset.TileWidth)
            let maxY = (int (max.Y + 0.5f) / tileset.TileHeight)
            
            let broadphaseTiles =
                [for x in minX..maxX do
                     for y in minY..maxY do
                         let tileaabb = TiledMapTileset.toAABB x y tileset
                         let tileId = TiledMapTileLayer.getTileId x y tileLayer
                         
                         yield tileId, tileaabb, x, y]
            
            broadphaseTiles


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
        
        
module Speculative =
    let speculativeSolver (dt: float32) (contact: Contact) =
        let normal = -contact.normal
        
        let nv = Vector2.Dot(contact.b.velocity - contact.a.velocity, normal)
        
        if nv > 0.f then
            contact
        else
            let remove = nv + (contact.distance / dt)
            let impulse = remove / (contact.a.inverseMass + contact.b.inverseMass)
            
            let newImpule = min (impulse + contact.impulse) 0.f
            
            let change = newImpule - contact.impulse
            
            {contact with
                 a = {contact.a with
                        velocity = contact.a.velocity + change * normal * contact.a.inverseMass}
                 b = {contact.b with
                        velocity = contact.b.velocity - change * normal * contact.b.inverseMass}
                 impulse = newImpule}
        
        
module Collision =
    
    let isInternalCollision (tileX: int) (tileY: int) (normal: Vector2) (tileLayer: TiledMapTileLayer) =
        let nextTileX = tileX + int normal.X
        let nextTileY = tileY + int normal.Y
        
        let currentTile = TiledMapTileLayer.getTileId tileX tileY tileLayer
        let nextTile = TiledMapTileLayer.getTileId nextTileX nextTileY tileLayer
        
        match nextTile with None -> false | Some _ -> true
        
    
    let AABBvAABB (a: RigidBody) (b: RigidBody) tileX tileY (map: TiledMapTileLayer) =
        let combinedExtents = b.aabb.halfExtents + a.aabb.halfExtents
        let delta = b.aabb.center - a.aabb.center
        
        let normal = delta.MajorAxis() |> Vector2.Negate
        let planeCenter = (normal * combinedExtents) + b.aabb.center
        
        let planeDelta = a.aabb.center - planeCenter
        let dist = Vector2.Dot(planeDelta, normal)
        
        let contact = Contact.create(a, b, normal, dist)
        let internalCollision = isInternalCollision tileX tileY normal map
        
        not internalCollision, contact
        
    let collisionResponse (moveableObject: RigidBody) (other: RigidBody) (contact: Contact) (dt: float32) =
        let solved = Speculative.speculativeSolver dt contact
        
        let tangent = solved.normal.PerpendicularCounterClockwise()
        
        let newVelocity = solved.a.velocity
        
        newVelocity, contact
        
    
    let innerCollide (tileLayer: TiledMapTileLayer) (moveableObject: RigidBody) (tileAabb: AABB) (tileType: int option) (dt: float32) (x: int) (y: int) =
        match tileType with
        | None -> None
        | tileType ->
            let tileRigidBody = RigidBody.create(0.f, tileAabb.size.X, tileAabb.size.Y, tileAabb.center, Vector2.Zero)
            let collision, contact = AABBvAABB moveableObject tileRigidBody x y tileLayer
            
            if collision then
                Some (collisionResponse moveableObject tileRigidBody contact dt)
            else None
                
    
    let collision (map: TiledMapTileLayer) (tileset: TiledMapTileset) (rigidBody: RigidBody) (dt: float32) =
        let expand = Vector2(5.f,5.f)
        let predictedPos = rigidBody.aabb.center + (rigidBody.velocity * dt)
        
        // find min/max
        let mutable min = Vector2.Min(rigidBody.aabb.center, predictedPos)
        let mutable max = Vector2.Max(rigidBody.aabb.center, predictedPos)
        
        // extend by radius
        min <- min - rigidBody.aabb.halfExtents
        max <- max + rigidBody.aabb.halfExtents
        
        // extend more to deal with being close to boundary
        min <- min - expand
        max <- max + expand
        
        TiledMapTileLayer.getBroadphaseTiles map tileset min max
        |> List.choose(fun (tileid, tileaabb, x,y) -> innerCollide map rigidBody tileaabb tileid dt x y)
        
        
        
//type TileSet =
//    {
//        tilesWide: int
//        tilesHigh: int
//        tileWidth: int
//        tileHeight: int
//        texture: Texture2D
//        sourceRectangles: Rectangle array
//    }
//    
//module TileSet =
//    let CreateTileSet(tilesWide, tilesHigh, tileWidth, tileHeight, texture) =
//        let sourceRectangles =
//            [|
//                for y in 0..tilesHigh-1 do
//                    for x in 0..tilesWide-1 do
//                        yield Rectangle(x*tileWidth, x*tileHeight, tileWidth, tileHeight)
//            |]
//        
//        {
//            tilesWide = tilesWide
//            tilesHigh = tilesHigh
//            tileWidth = tileWidth
//            tileHeight = tileHeight
//            texture = texture
//            sourceRectangles = sourceRectangles
//        }
//        
//type TileLayer = {
//    tiles: int array
//    width: int
//    height: int
//    visible: bool
//}
//
//module TileLayer =
//    let getTileId x y (layer: TileLayer) =
//        match x, y with
//        | (x,y) when x < 0 || y < 0 -> None
//        | (x,y) when x > layer.width || y > layer.height -> None
//        | _ ->
//            let index = y * layer.width + x
//            match layer.tiles |> Array.tryItem index with
//            | Some tileId when tileId > 0 -> Some(tileId-1) //id is 1-based, 0 being empty cell
//            | _ -> None
//            
//    let vectorToCell (position: Vector2) (tileset: TileSet) =
//        Point(int position.X / tileset.tileWidth, int position.Y / tileset.tileHeight)
//        
//    let draw (spriteBatch: SpriteBatch, tileset: TileSet, camera: Camera, layer: TileLayer, game: Game) =
//        if not layer.visible then ()
//        else
//            let cameraPoint =
//                let location =
//                    Vector2(camera.Position.X - (float32 game.GraphicsDevice.Viewport.Width * 0.5f),
//                            camera.Position.Y - (float32 game.GraphicsDevice.Viewport.Height * 0.5f))
//                vectorToCell location tileset
//            
//            let viewPoint =
//                let location =
//                    Vector2(camera.Position.X + (float32 game.GraphicsDevice.Viewport.Width * 0.5f),
//                            camera.Position.Y + (float32 game.GraphicsDevice.Viewport.Height * 0.5f))
//                vectorToCell location tileset
//                
//            let minX, minY = max 0 (cameraPoint.X - 1), max 0 (cameraPoint.Y - 1)
//            let maxX, maxY = min (viewPoint.X + 1) layer.width - 1, min (viewPoint.Y + 1) layer.height - 1
//            
//            for y in minY..maxY do
//                for x in minX..maxX do
//                    match getTileId x y layer with
//                    | None -> ()
//                    | Some tile ->
//                        if tile = -1 then () else
//                        let destination = Rectangle(x * tileset.tileWidth, y * tileset.tileHeight, tileset.tileWidth, tileset.tileHeight)
//                        spriteBatch.Draw(tileset.texture, destination, Nullable.op_Implicit tileset.sourceRectangles.[tile], Color.White)
            
        

type Game1 () as this =
    inherit Game()
 
    let graphics = new GraphicsDeviceManager(this, PreferredBackBufferWidth = 800, PreferredBackBufferHeight = 600)
    let mutable spriteBatch = Unchecked.defaultof<_>
    let mutable playerSpriteSheet = Unchecked.defaultof<Texture2D>
    let mutable player = Unchecked.defaultof<AnimatedSprite>
    let mutable playerAnimations = Unchecked.defaultof<_>
    let mutable camera = Unchecked.defaultof<_>
//    let mutable tileset = Unchecked.defaultof<TileSet>
//    let mutable tilelayer = Unchecked.defaultof<TileLayer>
//    let mutable terrain = Unchecked.defaultof<Texture2D>
    
    let mutable map = Unchecked.defaultof<TiledMap>
    let mutable mapRenderer = Unchecked.defaultof<TiledMapRenderer>
    
    
    
    let gravity = Vector2(0.0f, 50.f)
    let maxSpeed = 350.f
    
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
//        terrain <- this.Content.Load<Texture2D>("0x72_DungeonTilesetII_v1.2")
//        tileset <- TileSet.CreateTileSet (16, 16, 16, 16, terrain)
        camera <- Camera(this.GraphicsDevice.Viewport)
        base.Initialize()
        
        

    override this.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        playerSpriteSheet <- this.Content.Load<Texture2D>("0x72_DungeonTilesetII_v1.2")
        map <- this.Content.Load<TiledMap>("Map1")
        mapRenderer <- new TiledMapRenderer(this.GraphicsDevice)
        
        
        let body = RigidBody.create(60.f, 16.f, 16.f, Vector2(100.f, 100.f), Vector2.Zero)
        player <- {
            texture = playerSpriteSheet
            animations = playerAnimations
            currentAnimationKey = AnimationKey.IdleDown
            isAnimating = false
            speed = 166.f
            jump = 600.f
            rigidBody = body
            facingRight = true
        }

        // TODO: use this.Content to load your game content here
 
    override this.Update (gameTime) =
        if (GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape))
        then this.Exit();
        
        let dt = float32 gameTime.ElapsedGameTime.TotalSeconds

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
                movementVector, false, walkingToIdle player.currentAnimationKey  
            else movementVector |> Vector2.Normalize, true, animationKey
        
        let newAnimation =
            if player.currentAnimationKey = animationKey then
                player |> AnimatedSprite.updateAnimation animationKey gameTime
            else
                player |> AnimatedSprite.resetAnimation animationKey
               
        let facingRight =
            if animationKey = AnimationKey.WalkLeft then
                false
            elif animationKey = AnimationKey.WalkRight then
                true
            else
                player.facingRight
                
        let rbUpdated = {player.rigidBody with
                            velocity = movementVector * player.speed}

        let afterCollisionVelocity =
            Collision.collision map.TileLayers.[2] map.Tilesets.[0] rbUpdated dt
            |> List.sortBy(fun (_,c) -> c.distance)
            |> List.tryHead
            |> function
                | None ->
                    rbUpdated.velocity
                | Some (velocity, contact) ->
                    velocity
                    
        let newPosition =
            let maxClamp =
                Vector2 (float32 (map.Width * map.TileWidth) - float32 player.rigidBody.aabb.halfExtents.X,
                         float32 (map.Height * map.TileHeight) - float32 player.rigidBody.aabb.halfExtents.Y)
                
            let pos = player.rigidBody.aabb.center + (afterCollisionVelocity * dt)
            Vector2.Clamp(pos, player.rigidBody.aabb.halfExtents, maxClamp)
            
        player <- { player with
                        rigidBody =
                            {player.rigidBody with
                                velocity = afterCollisionVelocity
                                aabb = {player.rigidBody.aabb with center = newPosition}
                            }
                        isAnimating = true
                        currentAnimationKey = animationKey
                        facingRight = facingRight
                        animations = player.animations |> Map.add animationKey newAnimation }
        
        camera.Update player.rigidBody.aabb.center
        
        mapRenderer.Update(map, gameTime)
        
        base.Update(gameTime)
 
    override this.Draw (gameTime) =
        this.GraphicsDevice.Clear Color.CornflowerBlue

        // TODO: Add your drawing code here
        spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.AlphaBlend, SamplerState.PointClamp, transformMatrix = Nullable.op_Implicit camera.WorldToScreen)
        //player.Draw(spriteBatch)
        AnimatedSprite.draw player gameTime spriteBatch
        
        mapRenderer.Draw(map, Nullable.op_Implicit camera.WorldToScreen)
        spriteBatch.End()
        base.Draw(gameTime)

