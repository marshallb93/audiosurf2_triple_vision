---------------
-- FUNCTIONS --
---------------

function DoFunctionIf(test, if_true, if_false)
  if test then
      return if_true
  else
      return if_false
  end
end

function GetVisibility()
    if GetQualityLevel() == 1 then
        return 100
    elseif GetQualityLevel() == 2 then
        return 150
    elseif GetQualityLevel() == 3 then
        return 200
    else
        return 250
    end
end

function DeepCopy(orig) -- a pretty standard lua function for making a copy of a table
    local orig_type = type(orig)
    local copy
    if orig_type == 'table' then
        copy = {}
        for orig_key, orig_value in next, orig, nil do
            copy[DeepCopy(orig_key)] = DeepCopy(orig_value)
        end
        setmetatable(copy, DeepCopy(getmetatable(orig)))
    else -- number, string, boolean, etc
    copy = orig
    end
    return copy
end

function CreatePlayers()
    local players = {}

    local default_player = {
        score = 0,
        prevInput = {},
        iPrevRing = 0,
        hasFinishedScoringPrevRing = false,
        prevFirstBlockCollisionTested = 1,
        pos = {0, 0, 1.5},
        posCosmetic = {0, 0, 1.5},
        hits = 0,
    }

    local player_specific = {
        {
            uniqueName = "Left",
            thrusterRight = "thrusterright1",
            thrusterLeft = "thrusterleft1",
            controller = "key",
        },
        {
            uniqueName = "Centre",
            thrusterRight = "thrusterright2",
            thrusterLeft = "thrusterleft2",
            controller = "key",
        },
        {
            uniqueName = "Right",
            thrusterRight = "thrusterright3",
            thrusterLeft = "thrusterleft3",
            controller = "mouse",
        },
    }

    for i = 1, 3 do
        players[i] = DeepCopy(default_player)
        players[i].uniqueName = player_specific[i].uniqueName
        players[i].thrusterRight = player_specific[i].thrusterRight
        players[i].thrusterLeft = player_specific[i].thrusterLeft
        players[i].controller = player_specific[i].controller
        players[i].index = i
        players[i].minStrafe = (6 * i) - 15
        players[i].maxStrafe = (6 * i) - 9
    end

    return players
end

players = CreatePlayers()

--------------
-- SETTINGS --
--------------

GameplaySettings {

    -- ship settings
    usepuzzlegrid = false, -- useless for mono
    jumpmode = "none", -- no jump
    -- end ship settings

    -- block settings
    greypercent = 0, -- no grey blocks
    colorcount = 2, -- two colours, one for middle, one for outside
    usetraffic = true, -- we want traffic (blocks)
    automatic_traffic_collisions = false, -- collisions will be manually calculated
    trafficcompression = 0.65,
    -- end block settings

    -- track generation settings
    gravity = -0.45, -- sculpts track steepness
    playerminspeed = 0.1, -- player should always move
    playermaxspeed = 4, -- upper bound for difficulty
    minimumbestjumptime = 2.5, -- massage the track until a jump of at least this duration is possible
    uphilltiltscaler = 1.5, -- steeper uphills
    downhilltiltscaler = 1.5, -- steeper downhills
    uphilltiltsmoother = 0.02,
    downhilltiltsmoother = 0.04,
    useadvancedsteepalgorithm = true, -- more extreme track
    alldownhill = false, -- some uphill
    -- end track generation settings
}

SetSkinProperties {
	lanedividers = {-7.5,-4.5,-1.5,1.5,4.5,7.5},
	shoulderlines = {-10.5,10.5},
	trackwidth = 11.5,
}

SetBlocks {
    maxvisiblecount = GetVisibility() -- set blocks visible dependent on quality level
}

-------------
-- SCRIPTS --
-------------

function OnRequestLoadObjects()
 
    local ship_names = {"ship", "shipinverted"}
    local ship_shaders = {"highway", "highwayinverted"}
    local thruster_names = {"thrusterleft", "thrusterright", "thrusterleftinverted", "thrusterrightinverted"}
    local thruster_shaders = {"VertexColorUnlitTintedAddSmooth", "VertexColorInvertUnlitTintedAddSmooth.shader"}
    local thruster_positions = {
        {
            pos = {-0.175, 0.21, -1.297},
            rot = {0, 0, 313.7366},
            scale = {.7, .7, .7},
        },
        {
            pos = {.175, 0.21, -1.297},
            rot = {0, 0, 58.713},
            scale = {.7, .7, .7},
        },
    }
    for i = 1, 2 do
        CreateObject { -- create two ships (normal and inverted)
            name = ship_names[i],
            active = true,
            visible = false,
            gameobject = {
                min_hover_height = 0.23,
                max_hover_height = 0.8,
                use_water_rooster = false,
                smooth_tilting = false,
                smooth_tilting_speed = 10,
                smooth_tilting_max_offset = -20,
                mesh = "ninjamono.obj",
                shader = "VertexColorUnlitTinted",
                layer = 14,
                renderqueue = 2000,
                texture = "ninjaMono.png",
                scale = {
                    x = 1,
                    y = 1,
                    z = 1
                },
                pos = {
                    x = 0,
                    y = 0,
                    z = 0,
                },
                shadercolors = {
                    _Color = {
                        colorsource = ship_shaders[i],
                        scaletype = "intensity",
                        minscaler = 1,
                        maxscaler = 1.5,
                    },
                },
                thrusters = {
                    crossSectionShape = {
                        {-0.35, -0.35, 0},
                        {-0.5, 0, 0},
                        {-0.35, 0.35, 0},
                        {0, 0.5, 0},
                        {0.35, 0.35, 0},
                        {0.5, 0, 0},
                        {0.35, -0.35, 0},
                    },
                    perShapeNodeColorScalers = {0.5, 1, 1, 1, 1, 1, 0.5},
                    shader = thruster_shaders[i],
                    layer = 14,
                    renderqueue = 3999,
                    colorscaler = 2,
                    extrusions = 22,
                    stretch = -0.1191,
                    updateseconds = 0.025,
                    instances = {
                        {
                            pos = {0, 0.46, -1.28},
                            rot = {0, 0, 0},
                            scale = {0.7, 0.7, 0.7},
                        }
                    }
                }
            }
        }
        
        for j = 1, 2 do
            local k = (2*(i-1)) + j
            CreateObject { -- create 4 thrusters
                name= thruster_names[k],
                active= true,
                visible = false,
                gameobject = {
                    min_hover_height= 0.23,
                    max_hover_height = 0.8,
                    scale = {x=1,y=1,z=1},
                    thrusters = {
                        crossSectionShape={{-.35,-.35,0},{-.5,0,0},{-.35,.35,0},{0,.5,0},{.35,.35,0},{.5,0,0},{.35,-.35,0}},
                        perShapeNodeColorScalers={.5,1,1,1,1,1,.5},
                        shader = thruster_shaders[i],
                        layer = 14,
                        renderqueue = 3999,
                        colorscaler = 2,
                        extrusions=22,
                        stretch=-0.1191,
                        updateseconds = 0.025,
                        instances = {
                            thruster_positions[j]
                        }
                    }
                }
            }
        end
    end
end

function CompareJumpTimes(a,b) -- used to sort the track nodes by jump duration
	return a.jumpairtime > b.jumpairtime
end

powernodes = powernodes or {}
antinodes = antinodes or {}
lowestaltitude = 9999
highestaltitude = -9999
lowestaltitude_node = 0
highestaltitude_node = 0
track = track or {}
trackSeconds = 0

------------------------
--- TRAFFIC CREATION ---
------------------------

function OnTrafficCreated(theTraffic)
    half_lanespace = lanespace / 2

    traffic = theTraffic
    for i=1,#traffic do
        traffic[i].origIndex = i

    end

    -- drop the weakest blocks
    table.sort(traffic, CompareTrafficStrengthASC)
    local dropPercentage = 0
    local bps = #traffic / trackSeconds
    local maxBps = 3
    local targetBps = 0
    local scaleBack = 0.78

    if bps > maxBps * scaleBack then
        targetBps  = maxBps * scaleBack
    else
        targetBps = bps * scaleBack
    end

    dropPercentage = 1 -  (targetBps * trackSeconds / #traffic)

    local bound = math.floor(#traffic * dropPercentage)
    for i=1, bound do
        table.remove(traffic, 1)
    end

    table.sort(traffic, CompareTrafficOrigIndexASC) -- return traffic to normal order

    trafficExt = {} -- array of extended traffic

    for i = 1, #traffic do -- transform into trafficExt array
    local lane = math.random(-1,1) -- choose traffic lanes

    if i > 1 then -- lane assignment easing
    local timesep = track[traffic[i].impactnode].seconds - track[traffic[i-1].impactnode].seconds -- time in seconds between current and previous block
    local prevlane = traffic[i-1].lane
    if timesep < 0.07 then  -- make sure blocks with time apart < 0.07 are in the same lane
    lane = prevlane
    elseif timesep < 0.15 then
        if math.abs(prevlane) == 1 then   -- make sure blocks with time apart < 0.15 are in an adjacent lane
        lane = math.random(math.min(prevlane, 0),math.max(prevlane, 0))
        end
    else
        lane = lane
    end
    end

    traffic[i].lane = lane

    local lanes = {-3, -1, 1, 3}
    local types = {4, 6, 7, 6}
    local strafes = {-9, -3, 3, 9}
    local n = 4

    if lane == 0 then -- the case where 3 blocks need to be added
    lanes = {-2, 0, 2}
    types = {6, 7, 6}
    strafes = {-6, 0, 6}
    n = 3
    elseif lane == -1 then
        types = {6, 7, 6, 4} -- the case where 4 blocks need to be added, but not like default
    end

    for j = 1, n do
        local k = #trafficExt+1
        trafficExt[k] = DeepCopy(traffic[i]) -- add blocks into trafficExt array
        trafficExt[k].lane = lanes[j]   -- set lanes
        trafficExt[k].type = types[j]   -- set types
        trafficExt[k].strafe = strafes[j] -- set strafe

        local strafe = trafficExt[k].strafe
        local offset = {strafe, 0, 0}
        local block = {} -- set up block to be copied into block arrays
        block.lane = trafficExt[k].lane
        block.hidden = false
        block.tested = {false, false, false}
        block.subtracted = false
        block.type = trafficExt[k].type
        if block.type == 7 then
            block.player = 1
        elseif block.type == 6 then
            if block.lane < 0 then
                block.player = 2
            else
                block.player = 3
            end
        else
            block.player = -1
        end
        blocks[#blocks+1] = DeepCopy(block)
        blockNodes[#blockNodes+1] = trafficExt[k].impactnode
        blockOffsets[#blockOffsets+1] = offset
        blockColors[#blockColors+1] = track[trafficExt[k].impactnode].color
    end
    end
    return trafficExt -- when you return a traffic table from this function the game will read and apply any changes you made
end

----------------------
--- TRACK CREATION ---
----------------------

function OnTrackCreated(theTrack) -- track is created before the traffic
	track = theTrack

	local songMinutes = track[#track].seconds / 60
    trackSeconds = track[#track].seconds
    
	for i=1,#track do
		track[i].jumpedOver = false -- if this node was jumped over by a higher proiority jump
		track[i].origIndex = i
		track[i].antiOver = false
	end

	-- find the best jumps path in this song
	local strack = DeepCopy(track)
	table.sort(strack, CompareJumpTimes)

	for i=1,#strack do
		if strack[i].jumpairtime >= 2.4 then --only consider jumps of at least this amount of air time
			if not track[strack[i].origIndex].jumpedOver then
				local flightPathClear = true
				local jumpEndSeconds = strack[i].seconds + strack[i].jumpairtime + 10
				for j=strack[i].origIndex, #track do --make sure a higher priority jump doesn't happen while this one would be airborne
					if track[j].seconds <= jumpEndSeconds then
						if track[j].jumpedOver then
							flightPathClear = false
						end
					else
						break
					end
				end
				if flightPathClear then
					if #powernodes < (songMinutes + 1) then -- allow about one power node per minute of music
						if strack[i].origIndex > 300 then
							powernodes[#powernodes+1] = strack[i].origIndex
						end
						jumpEndSeconds = strack[i].seconds + strack[i].jumpairtime + 10
						for j=strack[i].origIndex, #track do
							if track[j].seconds <= jumpEndSeconds then
								track[j].jumpedOver = true --mark this node as jumped over (a better jump took priority) so it is not marked as a powernode
							else
								break
							end
						end
					end
				end
			end
		end

		if strack[i].pos.y > highestaltitude then
			highestaltitude = strack[i].pos.y
			highestaltitude_node = i
		end
		if strack[i].pos.y < lowestaltitude then
			lowestaltitude = strack[i].pos.y
			lowestaltitude_node = i
		end
	end
end

function CompareTrafficStrengthASC(a,b)
	return a.strength < b.strength
end

function CompareTrafficSpanDESC(a,b)
	return a.span > b.span
end

function CompareTrafficOrigIndexASC(a,b)
	return a.origIndex < b.origIndex
end

lanespace = 3
half_lanespace = 1.5

blocks = blocks or {}
blockNodes = blockNodes or {}
blockOffsets = blockOffsets or {}
blockColors = blockColors or {}

function InsertLoopyLoop(theTrack, apexNode, circumference) -- default loop code
	circumference = math.floor(circumference)
	apexNode = math.floor(apexNode)
    local halfSize = math.floor(circumference / 2)

    if (apexNode < halfSize) or ((apexNode + halfSize) > #theTrack) then
    	return theTrack
    end

    local startRing = math.max(1,apexNode - halfSize)
    local endRing = math.min(#theTrack, apexNode + halfSize)
    local span = endRing - startRing
    local startTilt = theTrack[startRing].tilt
    local endOriginalTilt = theTrack[endRing].tilt
    local endOriginalPan = theTrack[endRing].pan
    local tiltDeltaOverEntireLoop = -360 + (endOriginalTilt - startTilt)
    local startPan = theTrack[startRing].pan
    local pan = startPan

	local panConstant = 40 -- make this number bigger if you have problems with loops running into themselves
    local panRate = panConstant / halfSize

    local panRejoinSpan = math.max(circumference*2, 200)
    local panRejoinNode = math.min(#theTrack, endRing + panRejoinSpan)

    if theTrack[panRejoinNode].pan > startPan then
    	panRate = -panRate -- the loop should bend towards the future track segments naturally
    end

    local midRing = startRing + halfSize + math.ceil(halfSize/10)

    for i = startRing+1, endRing do
        theTrack[i].tilt = startTilt + tiltDeltaOverEntireLoop * ((i - startRing) / span)

        if i==midRing then panRate = -panRate end

        pan = pan + panRate -- pan just a little while looping to make sure it doesn't run into itself
        theTrack[i].pan = pan
    end

    local panDeltaCascade = theTrack[endRing].pan - endOriginalPan
    local tiltDeltaCascade = theTrack[endRing].tilt - endOriginalTilt;
    for i = endRing + 1, #theTrack do
        theTrack[i].tilt = theTrack[i].tilt + tiltDeltaCascade
        theTrack[i].pan = theTrack[i].pan + panDeltaCascade
        theTrack[i].funkyrot = true
    end

    return theTrack
end

-- Using smoother corkscrews by jmmwetering from http://www.audio-surf.com/forum/index.php/topic,13154.0.html

function quadtween(t)
	t = t*2.0 
	if t < 1.0 then return 0.5*t*t end
	t = t - 1.0
	return (-0.5*(t*(t-2) - 1))
end

function exptween(t) 
	return (math.exp(1-t*t/(1-t*t)))
end

function sinetween(t)
	return (0.5*(1.0 + math.sin((t-0.5)*math.pi)))
end

function SmoothTween(startvalue, added, progress, tweenfunc)
	-- Smooth tweening between startvalue and startvalue+added.
	-- progress is a number between 0.0 and 1.0 to determine how far along the tweening we are
	-- tweenfunc is the function used to do the tweening (defaults to quadtween)
	tweenfunc = tweenfunc or quadtween
	return startvalue + added*tweenfunc(progress)
end

function InsertCorkscrew(track, startNode, endNode, direction, rotation) 
	startNode = math.floor(startNode)
	endNode = math.floor(endNode)

	if endNode < #track then
    
		local startRoll = track[startNode].roll
		local endOriginalRoll = track[endNode].roll
		local span = endNode - startNode
        local added = rotation
        if direction == 1 then
            added = added * -1
        end
		
		for i = startNode, endNode do
			track[i].roll = SmoothTween(startRoll, added, (i-startNode)/span)
			track[i].funkyrot = true
		end

		local rollDeltaCascade = track[endNode].roll - endOriginalRoll;

		for i = endNode + 1, #track do
			track[i].roll = track[i].roll + rollDeltaCascade
		end
	end

    return track
end

function OnRequestTrackReshaping(theTrack) 

	for i=1,#powernodes do
        local size = 100 + 100 * math.max(1,(theTrack[powernodes[i]].jumpairtime / 10))
        theTrack = InsertLoopyLoop(theTrack, powernodes[i], size)
        if i==1 or i==2 then -- double twist on the strongest 2 loops
            theTrack = InsertCorkscrew(theTrack, powernodes[i], powernodes[i]+size*1.7, i%2, 720) -- due to the i%2, every corkscrew switches direction
        else
            theTrack = InsertCorkscrew(theTrack, powernodes[i], powernodes[i]+size*.85, i%2, 360)
        end
	end
	track = theTrack
	return track
end

function OnSkinLoaded() -- called after OnTrafficCreated. The skin script has loaded content.

    local prefabNames = {"shipinverted", "thrusterrightinverted", "thrusterleftinverted", "ship", "thrusterright", "thrusterleft", "ship", "thrusterright", "thrusterleft"}
    for i = 1, #players do  -- spawn ships/thrusters
        CreateClone{name=players[i].uniqueName, prefabName=prefabNames[3*i-2], attachToTrackWithNodeOffset=-0.5, transform={pos=players[1].pos}}
        CreateClone{name=players[i].thrusterRight, prefabName=prefabNames[3*i-1], attachToTrackWithNodeOffset=-0.5, transform={pos=players[1].pos}}
        CreateClone{name=players[i].thrusterLeft, prefabName=prefabNames[3*i], attachToTrackWithNodeOffset=-0.5, transform={pos=players[1].pos}}
    end
    
    CreateClone{name="firstPlaceBlock", prefabName="block", attachToTrackWithNodeOffset=-2, transform={pos=players[1].pos}}

	HideBuiltinPlayerObjects() -- hide the game-controlled vehicle since we're using script-controlled vehicles instead. Also hides the game-controlled surfer
    
	SetCamera{ -- calling this function (even just once) overrides the camera settings from the skin script
		nearcam={
			pos={0,4,-3.50475},
			rot={38,0,0},
			strafiness = 0
		},
        farcam={
			pos={0,14.8,-6},
			rot={41,0,0},
			strafiness = 0
		}
	}	
end

score = 0 -- the global score shared by all players
iCurrentRing = 0 -- Update function keeps this current
blocksToHide = {}

---------------------------
--- COLLISION DETECTION ---
---------------------------

function SameLane(lane, index)
    return lane == blocks[index].lane
end

function WithinTolerence(location, index)
    local tolerence = {ahead = 0.5, behind = 0.5}
    return (location <= (blockNodes[index] + tolerence.ahead)) and (location >= (blockNodes[index] - tolerence.behind))
end

function Owned(player, index)
    return player.index == blocks[index].player
end

function ShouldCollide(player, lane, location, index)
    return SameLane(lane, index) and WithinTolerence(location, index) and Owned(player, index) and not blocks[index].hidden
end

function Collide(player, location)
	local strafe = player.pos[1]
	local lane = 0;

	local absStrafe = math.abs(strafe)
	for i = 2, 0, -1 do
		if absStrafe > ((lanespace * i) + half_lanespace) then
			lane = i+1
			break
		end
	end

	if strafe < 0 then
		lane = -lane
	end

	local maxRing = iCurrentRing + 2
    
    for i = player.prevFirstBlockCollisionTested, #blockNodes do
        if blockNodes[i] <= maxRing then -- don't check too far ahead
            if ShouldCollide(player, lane, location, i) then
                player.hits = player.hits + 1
                blocksToHide[#blocksToHide + 1] = i
                blocks[i].hidden = true
                SetLocalScore {name = player.uniqueName, score = player.hits}
            end
        else
            break
        end
    end
end

mouseSpeed = .35
cosmeticStrafeSpeed = 20
 
function UpdatePlayer(player, input, dt, tracklocation)
	local currInput = GetInput()
    local mouseInput = currInput["mouse"]
	local mouseHorizontal = mouseInput["x"]
	local keyHorizontal = input["Horizontal"]
    local keyHorizontal2 = input["Horizontal2"]
    local maxStrafe = player.maxStrafe
    local minStrafe = player.minStrafe
	if(player.controller=="mouse") then
        if (keyHorizontal ~= 0) or (keyHorizontal2 ~= 0) or (not singleController and player.num ~= 3) then
            player.controller = "key"
        end
	elseif mouseHorizontal~=0 then
		if player.num==3 or singleController then -- only player3 has the option of mouse control
			player.controller = "mouse"
		end
	end
	if player.controller=="mouse" then
		if dt>0 then --don't move when the game is paused
			player.pos[1] = math.min(maxStrafe, math.max(minStrafe, player.pos[1] + mouseHorizontal * mouseSpeed))
		end

		player.posCosmetic[1] = player.pos[1]
	else
        local playerLane = 0
		if keyHorizontal > 0.5 then playerLane = 1
		elseif keyHorizontal < -0.5 then playerLane = -1 end
        
        if keyHorizontal2 > 0.5 then playerLane = 1
		elseif keyHorizontal2 < -0.5 then playerLane = -1 end

		if player.num==2 then playerLane = playerLane - 2
		elseif player.num==3 then playerLane = playerLane + 2 end

		player.pos[1] = lanespace * playerLane
		player.posCosmetic[1] = player.posCosmetic[1] + cosmeticStrafeSpeed * dt * (player.pos[1] - player.posCosmetic[1])
	end

	SendCommand{command="SetTransform", name=player.uniqueName, param={pos=player.posCosmetic}}
    SendCommand{command="SetTransform", name=player.thrusterRight, param={pos=player.posCosmetic}}
    SendCommand{command="SetTransform", name=player.thrusterLeft, param={pos=player.posCosmetic}}

	Collide(player, tracklocation)

	player.prevInput = input
end


lastButtonState = false
lastKeyState = false
singleController = false
winner = 0
quarterSecondCounter = 0

function Update(dt, tracklocation, playerstrafe, input) --called every frame
	iCurrentRing = math.floor(tracklocation)
	local playersInput = input["players"]

    button = playersInput[1].button2
    key = input["keyboard"].b
    if button and lastButtonState == false then -- toggle between single and double gamepad mode
        singleController = not singleController
    end
    if key and lastKeyState == false then -- toggle between single and double gamepad mode
        singleController = not singleController
    end
    lastButtonState = button
    if key == nil then
        lastKeyState = false
    else
        lastKeyState = true
    end
    
	blocksToHide = {}

    if singleController then
        for i=1,#players do
            UpdatePlayer(players[i], playersInput[1], dt, tracklocation)
        end
    else
        for i=1,#players do
            UpdatePlayer(players[i], playersInput[i], dt, tracklocation)
        end
    end

    if #blocksToHide > 0 then
		HideTraffic(blocksToHide)
        local hiddenBlockID = blocksToHide[1]
		local blockType = blocks[hiddenBlockID].type
		FlashAirDebris{colorID= DoFunctionIf(blockType>100, 5, blockType), duration = DoFunctionIf(blockType>100, 1.2, .15), sizescaler = DoFunctionIf(blockType>100, 25.0, 5.0)}
	end
    
    score = players[1].score + players[2].score + players[3].score
    SetGlobalScore{score=score,showdelta=false}

    RumbleActiveGamepad(0,0)
  
    if winner ~=0 and singleController == false then
        SendCommand{command="SetTransform", name="firstPlaceBlock", param={pos=players[winner].posCosmetic, scale={0.5,0.5,0.5}}}
    else
        SendCommand{command="SetTransform", name="firstPlaceBlock", param={pos=players[1].posCosmetic, scale={0,0,0}}}
    end
    
    quarterSecondCounter = quarterSecondCounter + dt
	if quarterSecondCounter>.25 then
		quarterSecondCounter = 0 -- quarterSecondCounter - .25
		UpdateEachQuarterSecond()
	end
end

function GetLeaders()
    local leaders = {1}
    local max_hits = players[1].hits
    for i = 2, #players do
        if players[i].hits > max_hits then
            leaders = {i}
            max_hits = players[i].hits
        elseif players[i].hits == max_hits then
            leaders[#leaders+1] = i
        end
    end
end

function UpdateEachQuarterSecond() -- for things that need to run regularly, but not every frame
    
    leaders = GetLeaders()
    
end

function OnRequestFinalScoring()
    return {
		bonuses = {
            "Left:"..players[1].hits,
            "Centre:"..players[2].hits,
            "Right:"..players[3].hits,
        },
		finalscore = score
	}
end