/*
 * Copyright 2011-2013 Arx Libertatis Team (see the AUTHORS file)
 *
 * This file is part of Arx Libertatis.
 *
 * Arx Libertatis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Arx Libertatis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Arx Libertatis.  If not, see <http://www.gnu.org/licenses/>.
 */
/* Based on:
===========================================================================
ARX FATALIS GPL Source Code
Copyright (C) 1999-2010 Arkane Studios SA, a ZeniMax Media company.

This file is part of the Arx Fatalis GPL Source Code ('Arx Fatalis Source Code').

Arx Fatalis Source Code is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

Arx Fatalis Source Code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Arx Fatalis Source Code.  If not, see
<http://www.gnu.org/licenses/>.

In addition, the Arx Fatalis Source Code is also subject to certain additional terms. You should have received a copy of these
additional terms immediately following the terms and conditions of the GNU General Public License which accompanied the Arx
Fatalis Source Code. If not, please request a copy in writing from Arkane Studios at the address below.

If you have questions concerning this license or the applicable additional terms, you may contact in writing Arkane Studios, c/o
ZeniMax Media Inc., Suite 120, Rockville, Maryland 20850 USA.
===========================================================================
*/
// Code: Cyril Meynier
//
// Copyright (c) 1999-2000 ARKANE Studios SA. All rights reserved

#include "gui/Speech.h"

#include <cstdlib>
#include <cstdio>
#include <algorithm>

#include <boost/lexical_cast.hpp>

#include "animation/Animation.h"

#include "cinematic/CinematicController.h"

#include "core/Config.h"
#include "core/Core.h"
#include "core/Localisation.h"
#include "core/GameTime.h"

#include "game/EntityManager.h"
#include "game/NPC.h"
#include "game/Player.h"

#include "gui/Interface.h"
#include "gui/Text.h"
#include "gui/TextManager.h"

#include "graphics/Draw.h"
#include "graphics/Math.h"
#include "graphics/font/Font.h"

#include "io/resource/ResourcePath.h"
#include "io/log/Logger.h"

#include "platform/Time.h"

#include "scene/GameSound.h"
#include "scene/Interactive.h"

#include "script/ScriptEvent.h"

extern TextureContainer *	arx_logo_tc;

extern bool EXTERNALVIEW;
extern bool REQUEST_SPEECH_SKIP;

SUBTITLE subs[MAX_SPEECH];
ARX_SPEECH aspeech[MAX_ASPEECH];
Notification speech[MAX_SPEECH];

int activeSub = -1;


void ARX_SPEECH_Init() {

	for(size_t i = 0 ; i < MAX_SPEECH ; i++ )
		speech[i].clear();
}

static void ARX_SPEECH_MoveUp() {

	if(speech[0].timecreation != ArxInstant_ZERO)
		speech[0].text.clear();

	for(size_t j = 0; j < MAX_SPEECH - 1; j++) {
		speech[j] = speech[j+1];
	}

	speech[MAX_SPEECH-1].clear();
}

void ARX_SPEECH_ClearAll()
{
	for(size_t i = 0; i < MAX_SPEECH; i++) {

		if(speech[i].timecreation == ArxInstant_ZERO)
			continue;

		speech[i].clear();
	}
}

void ARX_SPEECH_Add(const std::string & text) {
	
	if(text.empty())
		return;
	
	ArxInstant now = arxtime.now();
	if(now == ArxInstant_ZERO) {
		now = ArxInstantMs(1);
	}
	
	if(speech[MAX_SPEECH - 1].timecreation != ArxInstant_ZERO) {
		ARX_SPEECH_MoveUp();
	}
	
	for(size_t i = 0; i < MAX_SPEECH; i++) {

		if(speech[i].timecreation != ArxInstant_ZERO)
			continue;
		
		// Sets creation time
		speech[i].timecreation = now;
		speech[i].duration = ArxDurationMs(2000 + text.length() * 60);
		speech[i].text = text;
		return;
	}
	
	LogInfo << "Failed to add speech: " << text;
}

static bool isLastSpeech(size_t index) {
	
	for(size_t i = index + 1; i < MAX_SPEECH; i++) {

		if(speech[i].timecreation == ArxInstant_ZERO)
			continue;

		if(!speech[i].text.empty())
			return false;
	}
	
	return true;
}

static void ARX_SPEECH_Render() {
	
	long igrec = 14;
	
	Vec2i sSize = hFontInBook->getTextSize("p");
	sSize.y *= 3;
	
	int iEnd = igrec + sSize.y;
	
	for(size_t i = 0; i < MAX_SPEECH; i++) {
		
		if(speech[i].timecreation == ArxInstant_ZERO || speech[i].text.empty()) {
			continue;
		}
		
		Rectf rect(
			Vec2f(120 * g_sizeRatio.x - 16 * minSizeRatio(), igrec),
			16 * minSizeRatio(),
			16 * minSizeRatio()
		);
		
		GRenderer->SetRenderState(Renderer::AlphaBlending, true);
		GRenderer->SetBlendFunc(BlendSrcAlpha, BlendInvSrcAlpha);
		
		EERIEDrawBitmap(rect, .00001f, arx_logo_tc, Color::white);
		
		igrec += ARX_UNICODE_DrawTextInRect(hFontInBook, Vec2f(120.f * g_sizeRatio.x, igrec), 500 * g_sizeRatio.x,
		                           ' ' + speech[i].text, Color::white, NULL);
		
		if(igrec > iEnd && !isLastSpeech(i)) {
			ARX_SPEECH_MoveUp();
			break;
		}
	}

	GRenderer->SetRenderState(Renderer::AlphaBlending, false);
}

void ARX_SPEECH_Check()
{
	bool bClear = false;
	long exist = 0;

	for(size_t i = 0; i < MAX_SPEECH; i++) {
		if(speech[i].timecreation == ArxInstant_ZERO)
			continue;
		
		ArxDuration elapsed = arxtime.now() - speech[i].timecreation;
		if(elapsed > speech[i].duration) {
			ARX_SPEECH_MoveUp();
			i--;
		} else {
			exist++;
		}

		bClear = true;
	}

	if(bClear && pTextManage) {
		pTextManage->Clear();
	}

	if(exist)
		ARX_SPEECH_Render();
}

void ARX_SPEECH_Launch_No_Unicode_Seek(const std::string & text, Entity * io_source) {
	
	long mood = ANIM_TALK_NEUTRAL;
	long speechnum = ARX_SPEECH_AddSpeech(io_source, text, mood, ARX_SPEECH_FLAG_NOTEXT);
	if(speechnum >= 0) {
		
		aspeech[speechnum].scrpos = -1;
		aspeech[speechnum].es = NULL;
		aspeech[speechnum].ioscript = io_source;
		aspeech[speechnum].flags = 0;
		
		CinematicSpeech acs;
		acs.type = ARX_CINE_SPEECH_NONE;
		aspeech[speechnum].cine = acs;
	}
}

static void ARX_CONVERSATION_CheckAcceleratedSpeech() {
	
	if(REQUEST_SPEECH_SKIP) {
		for(size_t i = 0; i < MAX_ASPEECH; i++) {
			if((aspeech[i].exist) && !(aspeech[i].flags & ARX_SPEECH_FLAG_UNBREAKABLE)) {
				aspeech[i].duration = ArxDuration_ZERO;
			}
		}
		REQUEST_SPEECH_SKIP = false;
	}
}

void ARX_SPEECH_FirstInit() {
	for(size_t i = 0 ; i < MAX_ASPEECH ; i++) {
		aspeech[i].clear();
	}
}

static long ARX_SPEECH_GetFree() {
	
	for(size_t i = 0; i < MAX_ASPEECH; i++) {
		if(!aspeech[i].exist) {
			aspeech[i].cine.type = ARX_CINE_SPEECH_NONE;
			return i;
		}
	}
	
	return -1;
}

static void ARX_SPEECH_Release(long i) {
	
	if(aspeech[i].exist) {
		
		ARX_SOUND_Stop(aspeech[i].sample);
		
		if(ValidIOAddress(aspeech[i].io) && aspeech[i].io->animlayer[2].cur_anim) {
			AcquireLastAnim(aspeech[i].io);
			aspeech[i].io->animlayer[2].cur_anim = NULL;
		}
		
		if(aspeech[i].subtitle) {
			aspeech[i].subtitle->clear();
		}
		aspeech[i].clear();
	}
}

void ARX_SPEECH_ReleaseIOSpeech(Entity * io) {
	
	for(size_t i = 0; i < MAX_ASPEECH; i++) {
		if(aspeech[i].exist && aspeech[i].io == io) {
			ARX_SPEECH_Release(i);
		}
	}
}

void ARX_SPEECH_Reset() {
	for(size_t i = 0; i < MAX_ASPEECH; i++) {
		ARX_SPEECH_Release(i);
	}
}

void ARX_SPEECH_ClearIOSpeech(Entity * io) {
	
	if(!io) {
		return;
	}
	
	for(size_t i = 0; i < MAX_ASPEECH; i++) {
		
		if(!aspeech[i].exist || aspeech[i].io != io) {
			continue;
		}
		
		EERIE_SCRIPT * es = aspeech[i].es;
		Entity * io = aspeech[i].ioscript;
		long scrpos = aspeech[i].scrpos;
		ARX_SPEECH_Release(i);
		
		if(es && ValidIOAddress(io)) {
			ScriptEvent::send(es, SM_EXECUTELINE, "", io, "", scrpos);
		}
	}
}

long ARX_SPEECH_AddSpeech(Entity * io, const std::string & data, long mood,
                          SpeechFlags flags) {
	
	if(data.empty()) {
		return -1;
	}
	
	ARX_SPEECH_ClearIOSpeech(io);
	
	long num = ARX_SPEECH_GetFree();
	if(num < 0) {
		return -1;
	}
	
	aspeech[num].exist = 1;
	aspeech[num].time_creation = arxtime.now();
	aspeech[num].io = io; // can be NULL
	aspeech[num].duration = ArxDurationMs(2000); // Minimum value
	aspeech[num].flags = flags;
	aspeech[num].sample = audio::INVALID_ID;
	aspeech[num].mood = mood;

	LogDebug("speech \"" << data << '"');
	
	res::path sample;
	
	// For non-conversation speech choose a random variant
	long count = getLocalisedKeyCount(data);  
	long variant = 1;

	if(count > 1) {
		do {
			variant = Random::get(1, count);
		} while(io->lastspeechflag == variant);
		io->lastspeechflag = checked_range_cast<short>(variant);
	}
		
	LogDebug(" -> " << variant << " / " << count);
		
	if(variant > 1) {
		sample = data + boost::lexical_cast<std::string>(variant);
	} else {
		sample = data;
	}
	
	Entity * source = (aspeech[num].flags & ARX_SPEECH_FLAG_OFFVOICE) ? NULL : io;
	aspeech[num].sample = ARX_SOUND_PlaySpeech(sample, source);
	
	if(aspeech[num].sample == ARX_SOUND_TOO_FAR) {
		aspeech[num].sample = audio::INVALID_ID;
	}

	//Next lines must be removed (use callback instead)
	aspeech[num].duration = ARX_SOUND_GetDuration(aspeech[num].sample);

	if ((io->ioflags & IO_NPC) && !(aspeech[num].flags & ARX_SPEECH_FLAG_OFFVOICE)) {
		float fDiv = aspeech[num].duration /= io->_npcdata->speakpitch;
		aspeech[num].duration = ArxDurationMs(fDiv);
	}

	if (aspeech[num].duration < ArxDurationMs(500))
		aspeech[num].duration = ArxDurationMs(2000);

	if(aspeech[num].sample != audio::INVALID_ID) {
		aspeech[num].subtitle = addSub(data, "string" + ((variant != 1) ? boost::lexical_cast<std::string>(variant) : ""), io->pos, aspeech[num].duration);
	}
	
	return num;
}

void ARX_SPEECH_Update() {
	
	ArxInstant now = arxtime.now();

	if(cinematicBorder.isActive() || BLOCK_PLAYER_CONTROLS)
		ARX_CONVERSATION_CheckAcceleratedSpeech();

	for(size_t i = 0; i < MAX_ASPEECH; i++) {
		if(!aspeech[i].exist)
			continue;

		Entity * io = aspeech[i].io;

		// updates animations
		if(io) {
			if(aspeech[i].flags & ARX_SPEECH_FLAG_OFFVOICE)
				ARX_SOUND_RefreshSpeechPosition(aspeech[i].sample);
			else
				ARX_SOUND_RefreshSpeechPosition(aspeech[i].sample, io);
			
			if((io != entities.player() || EXTERNALVIEW) && ValidIOAddress(io)) {
				
				if(!io->anims[aspeech[i].mood])
					aspeech[i].mood = ANIM_TALK_NEUTRAL;
				
				ANIM_HANDLE * anim = io->anims[aspeech[i].mood];
				if(anim) {
					AnimLayer & layer2 = io->animlayer[2];
					if(layer2.cur_anim != anim || (layer2.flags & EA_ANIMEND)) {
						changeAnimation(io, 2, anim);
					}
				}
			}
		}

		// checks finished speech
		if(now >= aspeech[i].time_creation + aspeech[i].duration) {
			EERIE_SCRIPT *es = aspeech[i].es;
			Entity *io = aspeech[i].ioscript;
			long scrpos = aspeech[i].scrpos;
			ARX_SPEECH_Release(i);

			if(es && ValidIOAddress(io)) {
				ScriptEvent::send(es, SM_EXECUTELINE, "", io, "", scrpos);
			}
		}
	}
}

static long getFreeSub() {
	
	for(size_t i = 0; i < MAX_ASPEECH; i++) {
		if(!subs[i].exist) {
			return i;
		}
	}
	
	return -1;
}

SUBTITLE *addSub(const std::string & key, const std::string & variant, Vec3f sourcePos, ArxDuration duration) {
	if(key.empty()) {
		return NULL;
	}
	
	std::string _output = getLocalisedKey(key, variant);
	
	if(!_output.empty()) {
		long num = getFreeSub();
		if(num < 0) {
			return NULL;
		}
		
		subs[num].clear();
		subs[num].exist = true;
		subs[num].sourcePos = sourcePos;
		subs[num].duration = duration;
		subs[num].text = _output;
		
		//in cinematics there is no valid source, switch subs right away
		if(isInCinematic()) {
			activeSub = num;
		}
		return &subs[num];
	} else {
		return NULL;
	}
}

void updateSubs(Rectf *rect) {
	
	//find the closest speech
	int closest = -1;
	if(!isInCinematic()) {
		float leastDist = std::numeric_limits<float>::max();
		for(size_t i = 0; i < MAX_ASPEECH; i++) {
			if(subs[i].exist) {
				float dist;
				if((dist = glm::distance(subs[i].sourcePos, player.pos)) < leastDist) {
					closest = i;
					leastDist = dist;
				}
			}
		}
	}
	
	if(closest >= 0) {
		if((closest != activeSub)) {
			activeSub = closest;
		}
	} else {
		if(activeSub < 0) { //no subtitle to show
			return;
		}
	}
	
	SUBTITLE *sub = &subs[activeSub];
	
	if(sub->text.empty())
		return;
	
	int lineHeight = hFontInBook->getLineHeight();
	
	float textFieldHeight = static_cast<float>(lineHeight * 3); //
	
	float absoluteTextFieldBottomY;
	if(rect) {
		absoluteTextFieldBottomY = rect->bottom;
	} else {
		absoluteTextFieldBottomY = g_size.height();
	}
	
	float absoluteCurrentTextY = absoluteTextFieldBottomY - sub->deltaY;
	
	//enlarge the clipping rectangle by two line heights to make room for fading
	Rect::Num y = checked_range_cast<Rect::Num>(absoluteTextFieldBottomY - lineHeight * 2 - textFieldHeight);
	Rect::Num h = checked_range_cast<Rect::Num>(absoluteTextFieldBottomY);
	
	Rect clippingRect(0, y + 1, g_size.width(), h);
	if(config.interface.limitSpeechWidth) {
		s32 w = std::min(g_size.width(), s32(640 * g_sizeRatio.y));
		clippingRect.left = (g_size.width() - w) / 2;
		clippingRect.right = (g_size.width() + w) / 2;
	}

	if(rect) {
		clippingRect = Rect(*rect);
	}
	
	float displayedTextHeight = (float)ARX_UNICODE_DrawTextInRect(
		hFontInBook,
		Vec2f(clippingRect.left + 10.f, absoluteCurrentTextY - lineHeight),
		clippingRect.right - 10.f,
		sub->text,
		Color::white,
		&clippingRect, true);
	
	displayedTextHeight += textFieldHeight;
	
	//update subtitle position
	float delay = toMs(g_platformTime.lastFrameDuration());
	if(sub->deltaY <= displayedTextHeight) {
		//vitesse du scroll
		float yPerDelay;
		
		if(sub->duration != ArxDuration_ZERO) {
			yPerDelay = (sub->yPerMsec = displayedTextHeight / sub->duration) * delay;
		} else {
			sub->duration = ArxDurationMs(4000);
			yPerDelay = (sub->yPerMsec = displayedTextHeight / 4000.0f) * delay;
		}
		sub->deltaY += yPerDelay;
	}
	
	//update deltaY for other active subs
	for(size_t i = 0; i < MAX_ASPEECH; i++) {
		if(!subs[i].exist || subs[i].text.empty() || i == activeSub) {
			continue;
		}
		subs[i].deltaY += subs[i].yPerMsec * delay;
	}
}
