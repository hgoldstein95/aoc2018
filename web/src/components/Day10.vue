<template>
  <v-container>
    <v-layout align-center justify-center>
      <v-card class="elevation-12 mt-3" v-if="dataAvailable">
        <v-responsive>
          <svg :height="skyDims.height" :width="skyDims.width">
            <filter id="starBlur">
              <feGaussianBlur in="SourceGraphic" stdDeviation="1.5" />
            </filter>
            <rect width="100%" height="100%" fill="black" />
            <template v-for="(p, index) in normalize(points)">
              <circle
                :key="index"
                :cx="p.x" :cy="p.y"
                r="5"
                fill="yellow"
                filter="url(#starBlur)"
              />
            </template>
          </svg>        
        </v-responsive>
        <v-card-actions>
          <h2 class="mx-4">{{ step }}</h2>
          <v-select
            :items="dimOptions"
            v-model="skyDims"
            item-text="text"
            item-value="{ text, width, height }"
            hint="Aspect Ratio"
            persistent-hint
            return-object
            class="ml-2"
          >
          </v-select>
          <v-btn flat @click="reset">RESET</v-btn>
          <v-spacer></v-spacer>
          <v-btn flat fab @click="stepBackward(1000)">-1000</v-btn>
          <v-btn flat fab @click="stepBackward(100)">-100</v-btn>
          <v-btn flat fab @click="stepBackward(1)">-1</v-btn>
          <v-btn flat fab @click="stepForward(1)">+1</v-btn>
          <v-btn flat fab @click="stepForward(100)">+100</v-btn>
          <v-btn flat fab @click="stepForward(1000)">+1000</v-btn>
        </v-card-actions>
      </v-card>
      <h2 class="headline" v-else>Connecting to server...</h2>
    </v-layout>
  </v-container>
</template>

<script lang="ts">
import Vue from "vue";

const BORDER_SIZE = 50;

interface Point { x: number; y: number; }
interface APIResponse { points: Point[]; step: number; }

export default Vue.extend({
  data() {
    const dimOptions = [
      { text: "2x1", width: 1200, height: 600 },
      { text: "4x1", width: 1200, height: 300 },
      { text: "4x3", width: 800, height: 600 },
      { text: "HD", width: 1080, height: 720 },
    ];

    return {
      dimOptions,
      dataAvailable: false,
      points: [] as Point[],
      step: 0,
      skyDims: dimOptions[0],
    };
  },
  methods: {
    processResponse(p: Promise<{ data: APIResponse }>) {
      p.then((resp) => {
        this.points = resp.data.points;
        this.step = resp.data.step;
        this.dataAvailable = true;
      }).catch((_) => this.dataAvailable = false);
    },
    stepForward(n: number) {
      this.processResponse(
        this.axios.post("http://localhost:8000/next", `count=${n}`),
      );
    },
    stepBackward(n: number) {
      this.processResponse(
        this.axios.post("http://localhost:8000/prev", `count=${n}`),
      );
    },
    reset() {
      this.processResponse(this.axios.post("http://localhost:8000/reset"));
    },
    processKey(k: any) {
      switch (k.code) {
        case "ArrowLeft":
          this.stepBackward(1);
          break;
        case "ArrowRight":
          this.stepForward(1);
          break;
      }
    },
    normalize(pts: Point[]): Point[] {
      if (pts.length === 0) {
        return pts;
      }

      const xs = pts.map((p) => p.x);
      const ys = pts.map((p) => p.y);
      const xmax  = Math.max(...xs);
      const xmin  = Math.min(...xs);
      const ymax  = Math.max(...ys);
      const ymin  = Math.min(...ys);

      const xslope = (this.skyDims.width - (2 * BORDER_SIZE)) / (xmax - xmin);
      const yslope = (this.skyDims.height - (2 * BORDER_SIZE)) / (ymax - ymin);

      const transform = (p: Point) => {
        return {
          x: Math.round(BORDER_SIZE + xslope * (p.x - xmin)),
          y: Math.round(BORDER_SIZE + yslope * (p.y - ymin)),
        };
      };

      return pts.map(transform);
    },
  },
  mounted() {
    this.processResponse(this.axios.get("http://localhost:8000/curr"));
  },
  created() {
    window.addEventListener("keydown", this.processKey);
  },
  beforeDestroy() {
    window.removeEventListener("keydown", this.processKey);
  },
});
</script>
