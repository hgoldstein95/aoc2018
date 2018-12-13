<template>
  <div id="app">
    <v-app>
      <v-content>
        <v-toolbar color="primary mb-5" dark>
          <v-toolbar-title>
            <router-link tag="span" to="/" class="clicky">
              Advent of Code 2018
            </router-link>
          </v-toolbar-title>
          <v-spacer></v-spacer>
          <v-toolbar-items class="hidden-sm-and-down">
            <v-btn flat>
              <router-link tag="span" to="/day-10">Day 10</router-link>
            </v-btn>
          </v-toolbar-items>
        </v-toolbar>
        <router-view></router-view>
      </v-content>
    </v-app>
  </div>
</template>

<script lang="ts">
import Vue from "vue";
import VueRouter from "vue-router";

import Home from "@/components/Home.vue";
import Day10 from "@/components/Day10.vue";

const router = new VueRouter({
  routes: [
    { path: "/", component: Home },
    { path: "/day-10", component: Day10 },
  ],
});

export default Vue.extend({
  name: "app",
  router,
});
</script>

<style scoped>
.clicky {
  cursor: pointer;
}
</style>
