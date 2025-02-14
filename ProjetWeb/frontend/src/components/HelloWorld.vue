<script setup lang="ts">
import { init, text, selectedImageIndex } from './http-api.ts';

onMounted(() => {
   init();
})
</script>

<template>
  <h1>Images</h1>

  <select v-model="selectedImageIndex" @change="handleImageSelection">
    <option value="" disabled>--Please choose an image--</option>
    <option v-for="(item, index) in text" :key="index" :value="index">
      {{ item }}
    </option>
  </select>
  <div class="images-container"></div>

</template>

<style scoped>
.read-the-docs {
  color: #888;
}
</style>
