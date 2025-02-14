package pdl.backend;

import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Repository;

@Repository
public class ImageDao implements Dao<Image> {

  private final Map<Long, Image> images = new HashMap<>();

  public ImageDao() {
    final ClassPathResource imgFile1 = new ClassPathResource("logo.jpg");
	final ClassPathResource imgFile2 = new ClassPathResource("zig.jpg");
    byte[] fileContent1;
	byte[] fileContent2;
    try {
      fileContent1 = Files.readAllBytes(imgFile1.getFile().toPath());
	  fileContent2 = Files.readAllBytes(imgFile2.getFile().toPath());
	  Image img1 = new Image("logo.jpg", fileContent1);
	  Image img2 = new Image("zig.jpg", fileContent2);
      images.put(img1.getId(), img1);
	  images.put(img2.getId(), img2);
    } catch (final IOException e) {
      e.printStackTrace();
    }
  }

  @Override
  public Optional<Image> retrieve(final long id) {
	  return Optional.ofNullable(this.images.get(Long.valueOf(id)));
  }

  @Override
  public List<Image> retrieveAll() {
	  return new ArrayList<Image>(this.images.values());
  }

  @Override
  public void create(final Image img) {
	  this.images.put(img.getId(), img);
  }

  @Override
  public void update(final Image img, final String[] params) {
    // Not used
  }

  @Override
  public void delete(final Image img) {
	  this.images.remove(img);
  }
}
