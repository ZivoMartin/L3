package pdl.backend;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import org.springframework.http.HttpStatus;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.mock.web.MockMultipartFile;
import java.nio.file.Files;
import java.nio.file.Paths;

@SpringBootTest
@AutoConfigureMockMvc
@TestMethodOrder(OrderAnnotation.class)
public class ImageControllerTests {

	
	@Autowired
	private MockMvc mockMvc;

	@BeforeAll
	public static void reset() {
  	// reset Image class static counter
  	ReflectionTestUtils.setField(Image.class, "count", Long.valueOf(0));
	}

	@Test
	@Order(1)
	public void getImageListShouldReturnSuccess() throws Exception {
		this.mockMvc.perform(get("/images")).andExpect(status().isOk());
	}

	@Test
	@Order(2)
	public void getImageShouldReturnNotFound() throws Exception {
		this.mockMvc.perform(get("/images/10000000")).andExpect(status().isNotFound());
	}

	@Test
	@Order(3)
	public void getImageShouldReturnSuccess() throws Exception {
		this.mockMvc.perform(get("/images/0")).andExpect(status().isOk());
	}

	@Test
	@Order(4)
	public void deleteImagesShouldReturnMethodNotAllowed() throws Exception {
		// TODO
	}

	@Test
	@Order(5)
	public void deleteImageShouldReturnNotFound() throws Exception {
		this.mockMvc.perform(delete("/images/10000000")).andExpect(status().isNotFound());
	}

	@Test
	@Order(6)
	public void deleteImageShouldReturnSuccess() throws Exception {
	 	this.mockMvc.perform(delete("/images/0")).andExpect(status().isOk());
	}

	@Test
	@Order(7)
	public void createImageShouldReturnSuccess() throws Exception {
		  byte[] fileContent = Files.readAllBytes(Paths.get("zig.jpg"));  
		  
		  MockMultipartFile mockFile = new MockMultipartFile(
			  "file",  
			  "zig.jpg",  
			  "jpg",  
			  fileContent 
			  );
		  this.mockMvc.perform(multipart("http://localhost:8080/images")
							   .file(mockFile))  
			  .andExpect(status().isOk());  
	}

	@Test
	@Order(8)
	public void createImageShouldReturnUnsupportedMediaType() throws Exception {
		MockMultipartFile mockFile = new MockMultipartFile(
            "file",  
            "zig.webp",  
            "webp",  
            new byte[0]
			);

		 this.mockMvc.perform(multipart("http://localhost:8080/images")
            .file(mockFile))
            .andExpect(status().isUnsupportedMediaType()); 
	}
	
}
