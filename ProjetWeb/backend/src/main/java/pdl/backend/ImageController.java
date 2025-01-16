package pdl.backend;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Optional;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@RestController
public class ImageController {

  @Autowired
  private ObjectMapper mapper;

  private final ImageDao imageDao;

  @Autowired
  public ImageController(ImageDao imageDao) {
    this.imageDao = imageDao;
  }

  @RequestMapping(value = "/images/{id}", method = RequestMethod.GET, produces = MediaType.IMAGE_JPEG_VALUE)
  public ResponseEntity<?> getImage(@PathVariable("id") long id) {
	  var img = this.imageDao.retrieve(id);	 
	  if(img.isPresent()) {
		  return ResponseEntity.ok()
			  .contentType(MediaType.IMAGE_JPEG)
			  .body(img.get().getData());
	  } else {
		  return ResponseEntity
            .status(HttpStatus.NOT_FOUND)
            .body("Invalid id");
	  }

  }

  @RequestMapping(value = "/images/{id}", method = RequestMethod.DELETE)
  public ResponseEntity<?> deleteImage(@PathVariable("id") long id) {
	  var img = this.imageDao.retrieve(id);	 
	  if(img.isPresent()) {
		  this.imageDao.delete(img.get());
		  return ResponseEntity.ok().body("");
	  } else {
		  return ResponseEntity
            .status(HttpStatus.NOT_FOUND)
            .body("Invalid id");
	  }
  }

  @RequestMapping(value = "/images", method = RequestMethod.POST)
  public ResponseEntity<?> addImage(@RequestParam("file") MultipartFile file,							
      RedirectAttributes redirectAttributes) {
	  if (file.getContentType() != "jpg") {
		  System.out.println(file.getContentType());
		  return ResponseEntity
            .status(HttpStatus.UNSUPPORTED_MEDIA_TYPE)
            .body("Failed to read bytes");
	  }
	  try {
		  Image img = new Image(file.getName(), file.getBytes());
		  this.imageDao.create(img);
		  return ResponseEntity.ok().body("");
	  } catch (final IOException e) {
		   return ResponseEntity
            .status(HttpStatus.INTERNAL_SERVER_ERROR)
            .body("Failed to read bytes");
	  }
  }

  @RequestMapping(value = "/images", method = RequestMethod.GET, produces = "application/json; charset=UTF-8")
  @ResponseBody
  public ArrayNode getImageList() {
    ArrayNode nodes = mapper.createArrayNode();
    for (Image img: this.imageDao.retrieveAll())
		nodes.insert(Math.toIntExact(img.getId()), img.getName());
	
    return nodes;
  }

}
