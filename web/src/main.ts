import Vue from "vue";
import "./plugins/vuetify";
import App from "./App.vue";
import axios from "axios";
import VueAxios from "vue-axios";
import VueRouter from "vue-router";

Vue.config.productionTip = false;

Vue.use(VueAxios, axios);
Vue.use(VueRouter);

new Vue({
  render: (h) => h(App),
}).$mount("#app");
