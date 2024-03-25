using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class LookCamera : MonoBehaviour
{
    private Transform camera;
    private void Awake()
    {
        camera = GameObject.FindGameObjectWithTag("PlayerCamera").transform;
    }
    private void Update()
    {
        transform.LookAt(camera);
    }
}
