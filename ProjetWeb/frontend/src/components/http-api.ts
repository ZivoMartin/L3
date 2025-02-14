import { ref, onMounted } from 'vue';
import axios from 'axios';

export const text = ref<string[]>([]);
export const selectedImageIndex = ref<number | null>(null);

    
    const fetchTextData = async () => {
      try {
        const response = await axios.get('/images?ID=12345');
        text.value = response.data; // Assuming the response is an array of image URLs
      } catch (error) {
        console.error('Error fetching images:', error);
      }
    };

    const fetchAndSetImage = async (imageUrl: string) => {
      try {
        const response = await axios.get(imageUrl, { responseType: 'blob' });
        const reader = new FileReader();
        reader.readAsDataURL(response.data);
        reader.onload = () => {
          const imageEl = document.querySelector('img');
          if (imageEl) {
            imageEl.setAttribute('src', reader.result as string);
          }
        };
      } catch (error) {
        console.error('Error fetching image:', error);
      }
    };

    const handleImageSelection = () => {
      if (selectedImageIndex.value !== null) {
        const imageUrl = `/images/${selectedImageIndex.value}`;
        fetchAndSetImage(imageUrl);
      }
    };

    export const init = () =>    {       
      fetchTextData().then(() => {
          let index =0;
          text.value.forEach((elt) => {
                const imageUrl = `/images/${index}`;
          const imgEl = document.createElement('img');
          imgEl.src = imageUrl;
          imgEl.alt = elt;
          imgEl.style.height = '100px';
          imgEl.style.width = '200px';
          const container = document.querySelector('.images-container');
          if (container) {
          container.appendChild(imgEl);
          }
          index += 1;
          });
      });
    }   
   
