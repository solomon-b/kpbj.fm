#!/bin/bash
# Generate mock images for KPBJ 95.9FM staging data - Enhanced Version
# Fetches themed images from Unsplash and applies genre-specific post-processing
# Falls back to gradient generation if Unsplash fails

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
OUTPUT_DIR="$PROJECT_DIR/mock-data/media"
CACHE_DIR="$PROJECT_DIR/.image-cache"

# Create directories
mkdir -p "$OUTPUT_DIR/shows/logos"
mkdir -p "$OUTPUT_DIR/shows/banners"
mkdir -p "$OUTPUT_DIR/episodes/artwork"
mkdir -p "$OUTPUT_DIR/events/posters"
mkdir -p "$OUTPUT_DIR/blog/heroes"
mkdir -p "$OUTPUT_DIR/avatars"
mkdir -p "$CACHE_DIR"

# Configuration
USE_PHOTOS=${USE_PHOTOS:-true}  # Set to false to use gradients only
PHOTO_TIMEOUT=15
PHOTO_SOURCE=${PHOTO_SOURCE:-picsum}  # picsum or unsplash

# Genre to Unsplash keyword mappings
# Format: "genre|keywords|overlay_style"
# overlay_style: dark, light, warm, cool, neon, vintage, none
declare -A GENRE_UNSPLASH=(
    # Jazz - warm, intimate settings
    ["jazz"]="jazz,saxophone,piano|warm"
    ["jazz-fusion"]="jazz,concert,instruments|warm"

    # Soul/R&B/Funk - vibrant, soulful
    ["soul"]="soul,vintage,microphone|warm"
    ["rnb"]="neon,night,music|cool"
    ["funk"]="disco,dance,70s|vintage"
    ["disco"]="disco,ball,dance,party|neon"
    ["motown"]="vinyl,records,vintage|vintage"

    # Electronic - futuristic, digital
    ["electronic"]="synthesizer,electronic,neon|neon"
    ["techno"]="techno,rave,laser|dark"
    ["house"]="dj,club,dance|neon"
    ["deep-house"]="underwater,ocean,deep|cool"
    ["minimal"]="minimal,abstract,geometric|light"
    ["drum-and-bass"]="jungle,energy,bass|neon"
    ["trap"]="trap,urban,night|dark"
    ["industrial"]="industrial,factory,metal|dark"
    ["synthwave"]="retro,80s,neon,grid|neon"
    ["vaporwave"]="vaporwave,aesthetic,90s,palm|neon"
    ["witch-house"]="witch,dark,occult,forest|dark"

    # Rock variants
    ["rock"]="rock,guitar,concert|dark"
    ["indie"]="indie,cassette,vintage|vintage"
    ["indie-rock"]="guitar,band,indie|vintage"
    ["alternative"]="alternative,urban,grunge|dark"
    ["punk"]="punk,safety-pin,anarchist|dark"
    ["hardcore"]="hardcore,mosh,concert|dark"
    ["post-punk"]="post-punk,dark,wave|dark"
    ["garage"]="garage,band,raw|vintage"
    ["psychedelic"]="psychedelic,trippy,colorful|neon"
    ["shoegaze"]="fog,dreamy,ethereal,blur|light"
    ["post-rock"]="landscape,mountains,epic|cool"
    ["surf"]="surf,beach,ocean,wave|warm"
    ["britpop"]="london,british,90s|vintage"
    ["new-wave"]="new-wave,80s,synth|neon"
    ["emo"]="emo,dark,emotional|dark"

    # Metal variants
    ["metal"]="metal,heavy,dark,fire|dark"
    ["black-metal"]="forest,dark,winter,fog|dark"
    ["thrash"]="thrash,metal,speed|dark"
    ["doom"]="doom,heavy,dark,smoke|dark"
    ["stoner"]="desert,rock,cactus|warm"

    # Folk/Country/Acoustic
    ["folk"]="folk,acoustic,guitar,wood|warm"
    ["acoustic"]="acoustic,guitar,wooden|warm"
    ["bluegrass"]="banjo,country,barn|warm"
    ["country"]="country,western,ranch|warm"
    ["singer-songwriter"]="songwriter,acoustic,notebook|warm"

    # Classical/Opera
    ["classical"]="classical,orchestra,symphony|warm"
    ["baroque"]="baroque,palace,gold|warm"
    ["opera"]="opera,theatre,curtain|warm"
    ["chamber"]="chamber,string,quartet|warm"

    # World Music
    ["world"]="world,globe,ethnic|warm"
    ["afrobeat"]="africa,drums,colorful|warm"
    ["latin"]="latin,salsa,tropical|warm"
    ["cumbia"]="cumbia,latin,dance|warm"
    ["tropical"]="tropical,palm,beach|warm"
    ["reggae"]="reggae,jamaica,rasta|warm"
    ["dub"]="dub,soundsystem,bass|cool"
    ["ska"]="ska,checkered,dance|vintage"
    ["caribbean"]="caribbean,beach,tropical|warm"
    ["global-bass"]="global,bass,dance|neon"

    # Hip-Hop
    ["hip-hop"]="hip-hop,turntable,urban|dark"
    ["lo-fi"]="lofi,chill,anime,study|warm"

    # Blues
    ["blues"]="blues,guitar,smoky|warm"

    # Ambient/Experimental
    ["ambient"]="ambient,space,stars,nebula|cool"
    ["experimental"]="experimental,abstract,glitch|neon"
    ["drone"]="drone,space,dark,void|dark"
    ["noise"]="noise,static,distortion|dark"

    # Gospel
    ["gospel"]="gospel,church,choir|warm"

    # Eclectic/Variety
    ["eclectic"]="music,colorful,diverse|warm"
    ["variety"]="variety,collage,mix|warm"
    ["party"]="party,celebration,confetti|neon"
    ["chill"]="chill,relaxing,sunset|cool"
    ["morning"]="sunrise,morning,coffee|warm"
    ["night"]="night,city,lights|dark"
)

# Genre color palettes (gradient start, gradient end, text color, overlay opacity)
# Format: "start_color|end_color|text_color|overlay_opacity"
declare -A GENRE_COLORS=(
    # Jazz - warm golds and deep blues
    ["jazz"]="#1a365d|#d69e2e|#ffffff|0.4"
    ["jazz-fusion"]="#2c5282|#ed8936|#ffffff|0.4"

    # Soul/R&B/Funk - warm purples and golds
    ["soul"]="#553c9a|#d69e2e|#ffffff|0.4"
    ["rnb"]="#6b46c1|#ed64a6|#ffffff|0.5"
    ["funk"]="#9f7aea|#f6ad55|#1a202c|0.3"
    ["disco"]="#d53f8c|#ffd700|#1a202c|0.3"
    ["motown"]="#c05621|#fbd38d|#1a202c|0.3"

    # Electronic - neons and cybers
    ["electronic"]="#0d0d0d|#00ffff|#00ffff|0.6"
    ["techno"]="#1a1a2e|#e94560|#ffffff|0.6"
    ["house"]="#16213e|#ff6b6b|#ffffff|0.5"
    ["deep-house"]="#0f3460|#e94560|#ffffff|0.5"
    ["minimal"]="#2d3436|#636e72|#ffffff|0.3"
    ["drum-and-bass"]="#ff6b35|#1a1a2e|#ffffff|0.5"
    ["trap"]="#2d1b69|#ff006e|#ffffff|0.6"
    ["industrial"]="#1a1a1a|#8b0000|#ff0000|0.7"
    ["synthwave"]="#2b1055|#ff00ff|#00ffff|0.5"
    ["vaporwave"]="#ff71ce|#01cdfe|#ffffff|0.3"
    ["witch-house"]="#0d0d0d|#4a0080|#9370db|0.7"

    # Rock variants
    ["rock"]="#1a1a1a|#dc2626|#ffffff|0.5"
    ["indie"]="#4a5568|#68d391|#ffffff|0.4"
    ["indie-rock"]="#2d3748|#48bb78|#ffffff|0.4"
    ["alternative"]="#374151|#6366f1|#ffffff|0.5"
    ["punk"]="#000000|#ff1493|#ffff00|0.5"
    ["hardcore"]="#1a1a1a|#ff0000|#ffffff|0.6"
    ["post-punk"]="#1a1a1a|#7c3aed|#ffffff|0.6"
    ["garage"]="#d97706|#1f2937|#ffffff|0.4"
    ["psychedelic"]="#7c3aed|#f472b6|#fef08a|0.3"
    ["shoegaze"]="#c4b5fd|#fda4af|#1e1b4b|0.2"
    ["post-rock"]="#1e3a5f|#64748b|#ffffff|0.4"
    ["surf"]="#0891b2|#fde047|#1e3a8a|0.3"
    ["britpop"]="#dc2626|#1e40af|#ffffff|0.4"
    ["new-wave"]="#ec4899|#06b6d4|#ffffff|0.4"
    ["emo"]="#1f2937|#9333ea|#ffffff|0.5"

    # Metal variants
    ["metal"]="#0a0a0a|#4a0000|#ff0000|0.7"
    ["black-metal"]="#000000|#1a1a1a|#666666|0.8"
    ["thrash"]="#0a0a0a|#b91c1c|#fbbf24|0.6"
    ["doom"]="#1a1a1a|#4b0082|#9370db|0.7"
    ["stoner"]="#2e1065|#84cc16|#fbbf24|0.4"

    # Folk/Country/Acoustic
    ["folk"]="#78350f|#d97706|#fef3c7|0.3"
    ["acoustic"]="#92400e|#fbbf24|#ffffff|0.3"
    ["bluegrass"]="#365314|#a3e635|#ffffff|0.3"
    ["country"]="#78350f|#fcd34d|#ffffff|0.3"
    ["singer-songwriter"]="#7c2d12|#fdba74|#ffffff|0.3"

    # Classical/Opera
    ["classical"]="#1e1b4b|#c4b5fd|#fef3c7|0.3"
    ["baroque"]="#78350f|#fef3c7|#1e1b4b|0.2"
    ["opera"]="#7f1d1d|#fcd34d|#ffffff|0.4"
    ["chamber"]="#312e81|#e0e7ff|#1e1b4b|0.2"

    # World Music
    ["world"]="#166534|#fbbf24|#ffffff|0.3"
    ["afrobeat"]="#ea580c|#fde047|#1a1a1a|0.3"
    ["latin"]="#dc2626|#fbbf24|#ffffff|0.3"
    ["cumbia"]="#db2777|#fbbf24|#ffffff|0.3"
    ["tropical"]="#0d9488|#fde047|#1a1a1a|0.2"
    ["reggae"]="#15803d|#fbbf24|#dc2626|0.3"
    ["dub"]="#166534|#7c3aed|#ffffff|0.5"
    ["ska"]="#f97316|#000000|#ffffff|0.3"
    ["caribbean"]="#0891b2|#f97316|#ffffff|0.3"
    ["global-bass"]="#7c3aed|#22c55e|#ffffff|0.4"

    # Hip-Hop
    ["hip-hop"]="#1a1a1a|#f59e0b|#ffffff|0.5"
    ["lo-fi"]="#6366f1|#f9a8d4|#1e1b4b|0.3"

    # Blues
    ["blues"]="#1e3a8a|#60a5fa|#ffffff|0.4"

    # Ambient/Experimental
    ["ambient"]="#0f172a|#475569|#94a3b8|0.4"
    ["experimental"]="#0f0f0f|#a855f7|#22d3ee|0.5"
    ["drone"]="#0a0a0a|#1e293b|#64748b|0.6"
    ["noise"]="#1a1a1a|#ef4444|#ffffff|0.6"

    # Gospel
    ["gospel"]="#7c2d12|#fcd34d|#ffffff|0.3"

    # Eclectic/Variety
    ["eclectic"]="#4338ca|#f472b6|#fef08a|0.3"
    ["variety"]="#7c3aed|#22d3ee|#ffffff|0.3"
    ["party"]="#ec4899|#fbbf24|#1a1a1a|0.3"
    ["chill"]="#4338ca|#a5b4fc|#ffffff|0.3"
    ["morning"]="#fbbf24|#fb923c|#1a1a1a|0.2"
    ["night"]="#0f172a|#6366f1|#94a3b8|0.5"
)

# Show data: slug|title|genre-key
SHOWS=(
    "sunday-morning-jazz|Sunday Morning Jazz|jazz"
    "gospel-hour|Gospel Hour|gospel"
    "world-music-passport|World Music Passport|world"
    "classical-sundays|Classical Sundays|classical"
    "indie-mixtape|Indie Mixtape|indie"
    "soul-kitchen|Soul Kitchen|soul"
    "acoustic-sessions|Acoustic Sessions|acoustic"
    "electronic-sunday|Electronic Sunday|electronic"
    "reggae-vibes|Reggae Vibes|reggae"
    "blues-after-dark|Blues After Dark|blues"
    "experimental-sounds|Experimental Sounds|experimental"
    "midnight-ambient|Midnight Ambient|ambient"
    "monday-morning-wake-up|Monday Morning Wake Up|morning"
    "coffee-and-classics|Coffee & Classics|classical"
    "folk-tales|Folk Tales|folk"
    "midday-mix|Midday Mix|eclectic"
    "latin-grooves|Latin Grooves|latin"
    "rock-solid|Rock Solid|rock"
    "drive-time|Drive Time|variety"
    "hip-hop-fundamentals|Hip-Hop Fundamentals|hip-hop"
    "jazz-lounge|Jazz Lounge|jazz"
    "punk-power-hour|Punk Power Hour|punk"
    "metal-madness|Metal Madness|metal"
    "late-night-chill|Late Night Chill|chill"
    "graveyard-shift|Graveyard Shift|night"
    "tuesday-sunrise|Tuesday Sunrise|morning"
    "morning-brew|Morning Brew|morning"
    "singer-songwriter-showcase|Singer-Songwriter Showcase|singer-songwriter"
    "world-beat|World Beat|world"
    "indie-rock-hour|Indie Rock Hour|indie-rock"
    "soul-stirrers|Soul Stirrers|soul"
    "alternative-nation|Alternative Nation|alternative"
    "electronic-evolution|Electronic Evolution|electronic"
    "country-roads|Country Roads|country"
    "funk-sessions|Funk Sessions|funk"
    "jazz-standards|Jazz Standards|jazz"
    "noise-and-space|Noise & Space|noise"
    "deep-night-mix|Deep Night Mix|night"
    "midweek-morning|Midweek Morning|morning"
    "classical-interlude|Classical Interlude|classical"
    "folk-underground|Folk Underground|folk"
    "lunch-hour-favorites|Lunch Hour Favorites|eclectic"
    "afrobeat-express|Afrobeat Express|afrobeat"
    "psychedelic-journey|Psychedelic Journey|psychedelic"
    "hip-hop-heritage|Hip-Hop Heritage|hip-hop"
    "dub-vibrations|Dub Vibrations|dub"
    "shoegaze-and-dream-pop|Shoegaze & Dream Pop|shoegaze"
    "jazz-fusion|Jazz Fusion|jazz-fusion"
    "post-rock-horizons|Post-Rock Horizons|post-rock"
    "industrial-underground|Industrial Underground|industrial"
    "night-drones|Night Drones|drone"
    "thursday-wake-up-call|Thursday Wake-Up Call|morning"
    "baroque-and-beyond|Baroque & Beyond|baroque"
    "bluegrass-and-old-time|Bluegrass & Old Time|bluegrass"
    "tropical-sounds|Tropical Sounds|tropical"
    "garage-rock-revival|Garage Rock Revival|garage"
    "new-wave-nostalgia|New Wave Nostalgia|new-wave"
    "techno-territory|Techno Territory|techno"
    "ska-and-rocksteady|Ska & Rocksteady|ska"
    "r-and-b-slow-jams|R&B Slow Jams|rnb"
    "hardcore-punk|Hardcore Punk|hardcore"
    "doom-and-stoner|Doom & Stoner|doom"
    "witch-house|Witch House|witch-house"
    "late-night-jazz|Late Night Jazz|jazz"
    "minimal-techno|Minimal Techno|minimal"
    "friday-morning-groove|Friday Morning Groove|morning"
    "chamber-music|Chamber Music|chamber"
    "desert-rock|Desert Rock|stoner"
    "cumbia-and-chicha|Cumbia & Chicha|cumbia"
    "post-punk-power|Post-Punk Power|post-punk"
    "house-music-all-night|House Music All Night|house"
    "weekend-warm-up|Weekend Warm-Up|party"
    "trap-and-bass|Trap & Bass|trap"
    "disco-fever|Disco Fever|disco"
    "emo-night|Emo Night|emo"
    "black-metal|Black Metal|black-metal"
    "vaporwave-dreams|Vaporwave Dreams|vaporwave"
    "midnight-madness|Midnight Madness|party"
    "deep-house|Deep House|deep-house"
    "saturday-morning-cartoons|Saturday Morning Cartoons|variety"
    "opera-hour|Opera Hour|opera"
    "surf-rock|Surf Rock|surf"
    "global-bass|Global Bass|global-bass"
    "britpop-and-beyond|Britpop & Beyond|britpop"
    "drum-and-bass|Drum & Bass|drum-and-bass"
    "saturday-dance-party|Saturday Dance Party|party"
    "lo-fi-hip-hop|Lo-Fi Hip-Hop|lo-fi"
    "motown-and-stax|Motown & Stax|motown"
    "thrash-metal|Thrash Metal|thrash"
    "synthwave-night|Synthwave Night|synthwave"
    "ambient-soundscapes|Ambient Soundscapes|ambient"
    "late-night-beats|Late Night Beats|chill"
)

# Events data: slug|title|theme
EVENTS=(
    "summer-block-party-2025-08-08|Summer Block Party|party"
    "late-night-techno-dj-aurelia-2025-08-16|Late Night Techno: DJ Aurelia|techno"
    "record-fair-swap-meet-2025-08-22|Record Fair & Swap Meet|eclectic"
    "indie-rock-showcase-2025-08-30|Indie Rock Showcase|indie-rock"
    "kpbj-fundraiser-jazz-night-2025-09-05|KPBJ Fundraiser: Jazz Night|jazz"
    "all-ages-punk-matinee-2025-09-13|All Ages Punk Matinee|punk"
    "open-mic-night-2025-09-19|Open Mic Night|acoustic"
    "autumn-dj-showcase-2025-09-27|Autumn DJ Showcase|hip-hop"
    "halloween-costume-party-dance-night-2025-10-04|Halloween Costume Party|witch-house"
    "local-legends-portland-music-history-panel-2025-10-11|Local Legends: Portland Music History|folk"
    "acoustic-songwriter-circle-2025-10-18|Acoustic Songwriter Circle|singer-songwriter"
    "kpbj-fall-fundraiser-silent-auction-2025-10-25|KPBJ Fall Fundraiser|variety"
)

# Blog posts data: slug|title|theme
BLOG_POSTS=(
    "kpbj-wins-2025-community-radio-excellence-award|KPBJ Wins 2025 Community Radio Excellence Award|gospel"
    "behind-the-scenes-day-in-life-kpbj-host|Behind the Scenes: A Day in the Life of a KPBJ Host|jazz"
    "new-shows-premiering-fall-2025|New Shows Premiering This Fall|eclectic"
    "portland-underground-music-scene-venues-matter|Portland's Underground Music Scene|punk"
    "fundraising-update-summer-2025-pledge-drive|Fundraising Update: Summer Pledge Drive|soul"
    "discovering-hidden-gems-vinyl-shopping-portland|Discovering Hidden Gems: Vinyl Shopping|funk"
    "community-voices-listeners-shape-programming|Community Voices: Listeners Shape Programming|world"
    "looking-ahead-kpbj-plans-2026|Looking Ahead: KPBJ's Plans for 2026|synthwave"
)

# Host/User avatars data: slug|display_name|theme
AVATARS=(
    "admin|Admin|variety"
    "staff|Staff|eclectic"
    "vinylhead|VinylHead42|funk"
    "radiovolunteer|RadioVolunteer|folk"
    "midnightlistener|MidnightListener|ambient"
    "smooth-sullivan|Smooth Sullivan|jazz"
    "sister-joy|Sister Joy|gospel"
    "dj-wanderlust|DJ Wanderlust|world"
    "maestro-chen|Maestro Chen|classical"
    "cassette-kid|Cassette Kid|indie"
    "chef-groove|Chef Groove|soul"
    "willow-hart|Willow Hart|acoustic"
    "circuit-breaker|Circuit Breaker|electronic"
    "irie-lion|Irie Lion|reggae"
    "midnight-willie|Midnight Willie|blues"
    "the-sound-scientist|The Sound Scientist|experimental"
    "lunar-drift|Lunar Drift|ambient"
    "sunrise-sam|Sunrise Sam|morning"
    "dj-knowledge|DJ Knowledge|hip-hop"
    "spike-voltage|Spike Voltage|punk"
    "thor-the-destroyer|Thor the Destroyer|metal"
    "luna-waves|Luna Waves|chill"
    "cosmic-carl|Cosmic Carl|psychedelic"
    "bass-commander|Bass Commander|dub"
    "reverb-rachel|Reverb Rachel|shoegaze"
    "berlin-bob|Berlin Bob|techno"
    "neon-nancy|Neon Nancy|new-wave"
    "hex-hannah|Hex Hannah|witch-house"
    "disco-donna|Disco Donna|disco"
    "frost-fenrir|Frost Fenrir|black-metal"
    "aesthetic-andy|Aesthetic Andy|vaporwave"
    "jungle-jane|Jungle Jane|drum-and-bass"
    "retro-racer-rita|Retro Racer Rita|synthwave"
    "ambient-aurora|Ambient Aurora|ambient"
)

# Get Unsplash info for a genre
get_unsplash_info() {
    local genre="$1"
    local info="${GENRE_UNSPLASH[$genre]}"
    if [[ -z "$info" ]]; then
        info="${GENRE_UNSPLASH[eclectic]}"
    fi
    echo "$info"
}

# Get colors for a genre (with fallback)
get_colors() {
    local genre="$1"
    local colors="${GENRE_COLORS[$genre]}"
    if [[ -z "$colors" ]]; then
        colors="${GENRE_COLORS[eclectic]}"
    fi
    echo "$colors"
}

# Fetch image from photo service with caching
fetch_photo() {
    local keywords="$1"
    local width="$2"
    local height="$3"
    local seed="$4"
    local output="$5"

    # Create cache key from parameters
    local cache_key=$(echo "${PHOTO_SOURCE}-${width}-${height}-${seed}" | md5sum | cut -d' ' -f1)
    local cache_file="$CACHE_DIR/${cache_key}.jpg"

    # Check cache first
    if [[ -f "$cache_file" ]]; then
        cp "$cache_file" "$output"
        return 0
    fi

    local url
    case "$PHOTO_SOURCE" in
        picsum)
            # Lorem Picsum - reliable, uses seed for consistency
            url="https://picsum.photos/seed/${seed}/${width}/${height}"
            ;;
        unsplash)
            # Unsplash Source - themed by keywords (may be unreliable)
            url="https://source.unsplash.com/${width}x${height}/?${keywords}"
            ;;
        *)
            return 1
            ;;
    esac

    if curl -L -s --max-time $PHOTO_TIMEOUT "$url" -o "$output" 2>/dev/null; then
        # Verify it's a valid image (not HTML error page)
        if file "$output" | grep -qE "(JPEG|PNG|image)" && [[ $(stat -f%z "$output" 2>/dev/null || stat -c%s "$output") -gt 1000 ]]; then
            cp "$output" "$cache_file"
            return 0
        fi
    fi

    return 1
}

# Apply blur to abstract the photo
apply_blur() {
    local input="$1"
    local output="$2"
    local blur_amount="${3:-0x8}"  # Default blur radius

    convert "$input" -blur "$blur_amount" "$output"
}

# Apply overlay style to image
apply_overlay() {
    local input="$1"
    local output="$2"
    local style="$3"
    local color1="$4"
    local color2="$5"
    local opacity="$6"

    local size=$(identify -format '%wx%h' "$input")

    case "$style" in
        dark)
            # Dark overlay with some photo showing through
            convert "$input" \
                -blur 0x4 \
                -modulate 80,120,100 \
                \( -size "$size" "gradient:${color1}-${color2}" -alpha set -channel A -evaluate set 60% +channel \) \
                -compose overlay -composite \
                "$output"
            ;;
        light)
            convert "$input" \
                -blur 0x3 \
                \( -clone 0 -fill "rgba(255,255,255,$opacity)" -colorize 100% \) \
                -compose over -composite \
                -modulate 115,85,100 \
                "$output"
            ;;
        warm)
            convert "$input" \
                -blur 0x2 \
                -modulate 105,115,100 \
                \( -clone 0 -fill "rgba(255,160,80,$opacity)" -colorize 100% \) \
                -compose overlay -composite \
                "$output"
            ;;
        cool)
            convert "$input" \
                -blur 0x3 \
                -modulate 95,110,100 \
                \( -clone 0 -fill "rgba(80,130,255,$opacity)" -colorize 100% \) \
                -compose overlay -composite \
                "$output"
            ;;
        neon)
            # Strong color gradient overlay for electronic genres
            convert "$input" \
                -blur 0x6 \
                -modulate 90,140,100 \
                -contrast \
                \( -size "$size" \
                   -define gradient:angle=135 \
                   "gradient:${color1}-${color2}" \
                   -alpha set -channel A -evaluate set 70% +channel \) \
                -compose overlay -composite \
                "$output"
            ;;
        vintage)
            convert "$input" \
                -blur 0x2 \
                -modulate 100,75,100 \
                -fill "rgba(255,230,180,0.2)" -colorize 100% \
                \( -clone 0 -fill "rgba(100,70,30,$opacity)" -colorize 100% \) \
                -compose overlay -composite \
                "$output"
            ;;
        none)
            cp "$input" "$output"
            ;;
        *)
            cp "$input" "$output"
            ;;
    esac
}

# Add text overlay with shadow
add_text() {
    local input="$1"
    local output="$2"
    local text="$3"
    local font_size="$4"
    local text_color="$5"
    local gravity="${6:-center}"

    # Add text with shadow for readability
    convert "$input" \
        \( -clone 0 \
           -font "DejaVu-Sans-Bold" \
           -pointsize $font_size \
           -fill "rgba(0,0,0,0.5)" \
           -gravity "$gravity" \
           -annotate +3+3 "$text" \
        \) -composite \
        -font "DejaVu-Sans-Bold" \
        -pointsize $font_size \
        -fill "$text_color" \
        -gravity "$gravity" \
        -annotate 0 "$text" \
        -quality 85 \
        "$output"
}

# Generate gradient fallback
generate_gradient() {
    local output="$1"
    local width="$2"
    local height="$3"
    local color1="$4"
    local color2="$5"
    local angle="${6:-135}"

    convert -size "${width}x${height}" \
        -define gradient:angle=$angle \
        "gradient:${color1}-${color2}" \
        "$output"
}

# Add texture/pattern overlay
add_texture() {
    local input="$1"
    local output="$2"
    local texture_type="$3"

    local size=$(identify -format '%wx%h' "$input")

    case "$texture_type" in
        noise)
            convert "$input" \
                -attenuate 0.15 +noise Gaussian \
                "$output"
            ;;
        scanlines)
            convert "$input" \
                \( -size "$size" pattern:horizontal2 -alpha set -channel A -evaluate set 8% +channel \) \
                -compose over -composite \
                "$output"
            ;;
        grain)
            convert "$input" \
                \( -size "$size" xc: +noise Random -colorspace Gray -alpha set -channel A -evaluate set 5% +channel \) \
                -compose overlay -composite \
                "$output"
            ;;
        halftone)
            convert "$input" \
                -colorspace Gray \
                -ordered-dither h8x8a \
                -colorspace sRGB \
                "$output"
            ;;
        *)
            cp "$input" "$output"
            ;;
    esac
}

# Main image generation function
generate_image() {
    local slug="$1"
    local title="$2"
    local genre="$3"
    local width="$4"
    local height="$5"
    local output="$6"
    local image_type="${7:-logo}"  # logo, banner, episode, poster, hero, avatar

    IFS='|' read -r color1 color2 text_color opacity <<< "$(get_colors "$genre")"
    IFS='|' read -r keywords style <<< "$(get_unsplash_info "$genre")"

    local temp_file=$(mktemp --suffix=.jpg)
    local processed_file=$(mktemp --suffix=.jpg)
    local use_photo=false

    # Try to fetch photo if enabled
    if [[ "$USE_PHOTOS" == "true" ]]; then
        # Use slug as seed for consistent images per show
        local seed=$(echo "$slug" | md5sum | cut -d' ' -f1 | head -c 8)
        if fetch_photo "$keywords" "$width" "$height" "$seed" "$temp_file"; then
            use_photo=true
        fi
    fi

    # Fall back to gradient if photo fetch failed
    if [[ "$use_photo" == "false" ]]; then
        generate_gradient "$temp_file" "$width" "$height" "$color1" "$color2"
        style="none"
    fi

    # Apply overlay style
    apply_overlay "$temp_file" "$processed_file" "$style" "$color1" "$color2" "$opacity"

    # Add texture based on genre characteristics
    case "$genre" in
        synthwave|vaporwave|electronic|techno)
            add_texture "$processed_file" "$processed_file" "scanlines"
            ;;
        indie|folk|blues|country|vintage)
            add_texture "$processed_file" "$processed_file" "grain"
            ;;
        noise|industrial|metal)
            add_texture "$processed_file" "$processed_file" "noise"
            ;;
    esac

    # Calculate font size based on image type and title length
    local title_len=${#title}
    local font_size
    case "$image_type" in
        logo)
            font_size=48
            [[ $title_len -gt 20 ]] && font_size=36
            [[ $title_len -gt 30 ]] && font_size=28
            ;;
        banner)
            font_size=64
            [[ $title_len -gt 20 ]] && font_size=52
            [[ $title_len -gt 30 ]] && font_size=40
            ;;
        episode)
            font_size=72
            ;;
        poster)
            font_size=42
            [[ $title_len -gt 25 ]] && font_size=32
            [[ $title_len -gt 40 ]] && font_size=26
            ;;
        hero)
            font_size=48
            [[ $title_len -gt 30 ]] && font_size=36
            [[ $title_len -gt 50 ]] && font_size=28
            ;;
        avatar)
            # For avatars, use initials
            local initials=""
            local words=($title)
            for word in "${words[@]:0:2}"; do
                initials+="${word:0:1}"
            done
            title=$(echo "$initials" | tr '[:lower:]' '[:upper:]')
            font_size=96
            ;;
    esac

    # Copy processed file to output (no text overlay)
    cp "$processed_file" "$output"

    # Compress for smaller file size
    mogrify -quality 70 -strip "$output"

    # Cleanup
    rm -f "$temp_file" "$processed_file"
}

# Generate show logo (4:3 - 800x600)
generate_logo() {
    local slug="$1"
    local title="$2"
    local genre="$3"
    local output="$OUTPUT_DIR/shows/logos/${slug}.jpg"

    generate_image "$slug" "$title" "$genre" 800 600 "$output" "logo"
    echo "  Logo: $slug"
}

# Generate show banner (3:1 - 1200x400)
generate_banner() {
    local slug="$1"
    local title="$2"
    local genre="$3"
    local output="$OUTPUT_DIR/shows/banners/${slug}.jpg"

    generate_image "$slug" "$title" "$genre" 1200 400 "$output" "banner"
    echo "  Banner: $slug"
}

# Generate episode artwork (1:1 - 500x500)
generate_episode_art() {
    local show_slug="$1"
    local episode_num="$2"
    local genre="$3"
    local output="$OUTPUT_DIR/episodes/artwork/${show_slug}-ep${episode_num}.jpg"

    # Use episode number as part of seed for variation
    local ep_slug="${show_slug}-ep${episode_num}"
    generate_image "$ep_slug" "EP $episode_num" "$genre" 500 500 "$output" "episode"
    echo "  Episode Art: ${show_slug} EP${episode_num}"
}

# Generate event poster (3:4 - 600x800)
generate_event_poster() {
    local slug="$1"
    local title="$2"
    local theme="$3"
    local output="$OUTPUT_DIR/events/posters/${slug}.jpg"

    generate_image "$slug" "$title" "$theme" 600 800 "$output" "poster"
    echo "  Event Poster: $slug"
}

# Generate blog hero image (16:9 - 1200x675)
generate_blog_hero() {
    local slug="$1"
    local title="$2"
    local theme="$3"
    local output="$OUTPUT_DIR/blog/heroes/${slug}.jpg"

    generate_image "$slug" "$title" "$theme" 1200 675 "$output" "hero"
    echo "  Blog Hero: $slug"
}

# Generate user avatar (1:1 - 300x300)
generate_avatar() {
    local slug="$1"
    local display_name="$2"
    local theme="$3"
    local output="$OUTPUT_DIR/avatars/${slug}.jpg"

    generate_image "$slug" "$display_name" "$theme" 300 300 "$output" "avatar"
    echo "  Avatar: $slug"
}

# Main execution
echo "=== KPBJ 95.9FM Mock Image Generator (Enhanced) ==="
echo ""
echo "Configuration:"
echo "  Use Photos: $USE_PHOTOS"
echo "  Photo Source: $PHOTO_SOURCE"
echo "  Cache directory: $CACHE_DIR"
echo ""

echo "Generating show images..."
count=0
for show_data in "${SHOWS[@]}"; do
    IFS='|' read -r slug title genre <<< "$show_data"
    generate_logo "$slug" "$title" "$genre"
    generate_banner "$slug" "$title" "$genre"

    # Generate 3 episode artworks per show
    for ep in 1 2 3; do
        generate_episode_art "$slug" "$ep" "$genre"
    done

    count=$((count + 1))
done

echo ""
echo "Generating event posters..."
event_count=0
for event_data in "${EVENTS[@]}"; do
    IFS='|' read -r slug title theme <<< "$event_data"
    generate_event_poster "$slug" "$title" "$theme"
    event_count=$((event_count + 1))
done

echo ""
echo "Generating blog hero images..."
blog_count=0
for blog_data in "${BLOG_POSTS[@]}"; do
    IFS='|' read -r slug title theme <<< "$blog_data"
    generate_blog_hero "$slug" "$title" "$theme"
    blog_count=$((blog_count + 1))
done

echo ""
echo "Generating user avatars..."
avatar_count=0
for avatar_data in "${AVATARS[@]}"; do
    IFS='|' read -r slug display_name theme <<< "$avatar_data"
    generate_avatar "$slug" "$display_name" "$theme"
    avatar_count=$((avatar_count + 1))
done

echo ""
echo "=== Generation Complete ==="
echo ""
echo "Shows:"
echo "  Logos: $count (800x600, 4:3)"
echo "  Banners: $count (1200x400, 3:1)"
echo "  Episode artwork: $((count * 3)) (500x500, 1:1)"
echo ""
echo "Events:"
echo "  Posters: $event_count (600x800, 3:4)"
echo ""
echo "Blog:"
echo "  Hero images: $blog_count (1200x675, 16:9)"
echo ""
echo "Users:"
echo "  Avatars: $avatar_count (300x300, 1:1)"
echo ""
echo "Total images: $((count + count + count*3 + event_count + blog_count + avatar_count))"
echo "Output directory: $OUTPUT_DIR"
echo "Cache directory: $CACHE_DIR"
